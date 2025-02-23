import std/[tables, strutils, streams, options, bitops]
import nimsimd/[sse2]

type
  DesktopEntryError* = object of CatchableError

  GroupType* = enum
    gtDesktopEntry
    gtDesktopAction
    gtUnknown

  DesktopActionTable = Table[string, Table[string, string]]

  DesktopEntry* = ref object
    name*: string
    genericName*: string
    noDisplay*: bool
    comment*: string
    icon*: string
    exec*: string
    path*: string
    terminal*: bool
    `type`*: string
    categories*: seq[string]
    mimeTypes*: seq[string]
    keywords*: seq[string]
    actions*: DesktopActionTable
    extraFields*: Table[string, string]

const
  BufferSize = 4096
  SectionStart = '['
  SectionEnd = ']'
  KeyValueSep = '='
  CommentChar = '#'
  ValidTypes = ["Application", "Link", "Directory"]
  RequiredKeys = ["Name", "Type"]

proc validateType(entryType: string) =
  if entryType notin ValidTypes:
    raise newException(
      DesktopEntryError,
      "Invalid Type value: " & entryType & ". Must be one of: " & $ValidTypes,
    )

proc validateExec(exec: string) =
  if exec.len == 0:
    raise newException(DesktopEntryError, "Exec field cannot be empty")
  if not exec.contains('%'):
    return
  for i, c in exec:
    if c == '%':
      if i == exec.high:
        raise newException(DesktopEntryError, "Invalid Exec field: % at end of string")
      let code = exec[i + 1]
      if code notin {'f', 'F', 'u', 'U', 'i', 'c', 'k', 'v', 'm', '%'}:
        raise newException(DesktopEntryError, "Invalid field code in Exec: %" & code)

proc parseListSIMD(value: string): seq[string] {.inline.} =
  if value.len == 0:
    return

  var
    current = ""
    i = 0
    escaping = false

  while i < value.len:
    let c = value[i]

    if escaping:
      case c
      of 's':
        current.add(' ')
      of 'n':
        current.add('\n')
      of 'r':
        current.add('\r')
      of 't':
        current.add('\t')
      of ';':
        current.add(';')
      of '\\':
        current.add('\\')
      else:
        current.add(c)
      escaping = false
    else:
      case c
      of '\\':
        escaping = true
      of ';':
        result.add(current)
        current = ""
      else:
        current.add(c)

    inc i

  if current.len > 0:
    result.add(current)
  elif value.endsWith(";"):
    result.add("")

  return result

proc findLineEndingSIMD(
    data: ptr UncheckedArray[byte], start, length: int
): int {.inline.} =
  let searchLen = length - start
  if searchLen <= 0:
    return start

  var pos = start
  let alignedStart = (start + 15) and not 15
  let alignedEnd = (start + searchLen) and not 15

  while pos < alignedStart and pos < length:
    if data[pos] == ord('\n'):
      return pos
    inc pos

  let newline = mm_set1_epi8(10.int8)
  while pos < alignedEnd:
    let chunk = mm_loadu_si128(cast[ptr M128i](addr data[pos]))
    let matches = mm_cmpeq_epi8(chunk, newline)
    let mask = mm_movemask_epi8(matches)

    if mask != 0:
      return pos + countTrailingZeroBits(uint32(mask))

    pos += 16

  while pos < length:
    if data[pos] == ord('\n'):
      return pos
    inc pos

  result = length

proc validateGroup(group: GroupType, header: string) =
  case group
  of gtDesktopEntry:
    if header != "[Desktop Entry]":
      raise newException(
        DesktopEntryError, "First group must be [Desktop Entry], found: " & header
      )
  of gtDesktopAction:
    if not header.startsWith("[Desktop Action ") or not header.endsWith("]"):
      raise newException(
        DesktopEntryError, "Invalid Desktop Action group header: " & header
      )
  else:
    discard

proc validateLocaleString(value: string): bool =
  for c in value:
    if c notin {'a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', '_', '@', '.', '[', ']', '='}:
      return false
  return true

proc parseDesktopEntry*(filepath: string): DesktopEntry =
  var
    f = open(filepath, fmRead)
    buffer = newString(BufferSize)
    currentSection = ""
    currentAction = ""
    hasDesktopEntry = false
    foundRequiredKeys = initTable[string, bool]()

  for key in RequiredKeys:
    foundRequiredKeys[key] = false

  result = DesktopEntry(
    noDisplay: false,
    terminal: false,
    categories: @[],
    mimeTypes: @[],
    keywords: @[],
    actions: initTable[string, Table[string, string]](),
    extraFields: initTable[string, string](),
  )

  let firstLineBytes = readLine(f)
  validateGroup(gtDesktopEntry, firstLineBytes)
  hasDesktopEntry = true
  currentSection = "Desktop Entry"

  while true:
    let bytesRead = readBuffer(f, addr buffer[0], BufferSize)
    if bytesRead <= 0:
      break

    var
      lineStart = 0
      data = cast[ptr UncheckedArray[byte]](addr buffer[0])

    while lineStart < bytesRead:
      let lineEnd = findLineEndingSIMD(data, lineStart, bytesRead)
      if lineEnd <= lineStart:
        break

      var line = buffer[lineStart ..< lineEnd].strip()
      if line.len == 0 or line[0] == CommentChar:
        lineStart = lineEnd + 1
        continue

      if line[0] == SectionStart:
        let sectionEnd = line.find(SectionEnd)
        if sectionEnd <= 1:
          raise newException(DesktopEntryError, "Invalid section header: " & line)

        currentSection = line[1 ..< sectionEnd]
        if currentSection.startsWith("Desktop Action"):
          validateGroup(gtDesktopAction, line)
          currentAction = currentSection[14 ..^ 1]
          result.actions[currentAction] = initTable[string, string]()
      else:
        let sepPos = line.find(KeyValueSep)
        if sepPos <= 0:
          raise newException(DesktopEntryError, "Invalid key-value pair: " & line)

        let
          key = line[0 ..< sepPos].strip()
          rawValue = line[sepPos + 1 ..^ 1].strip()

        if not validateLocaleString(key):
          raise newException(DesktopEntryError, "Invalid characters in key: " & key)

        let value = rawValue.strip(leading = true, trailing = true, chars = {'\'', '"'})

        if currentSection == "Desktop Entry":
          case key.toLowerAscii()
          of "name":
            result.name = value
            foundRequiredKeys["Name"] = true
          of "genericname":
            result.genericName = value
          of "nodisplay":
            result.noDisplay = value.toLowerAscii() in ["true", "1"]
          of "comment":
            result.comment = value
          of "icon":
            result.icon = value
          of "exec":
            validateExec(value)
            result.exec = value
          of "path":
            result.path = value
          of "terminal":
            result.terminal = value.toLowerAscii() in ["true", "1"]
          of "type":
            validateType(value)
            result.`type` = value
            foundRequiredKeys["Type"] = true
          of "categories":
            result.categories = parseListSIMD(value)
          of "mimetypes":
            result.mimeTypes = parseListSIMD(value)
          of "keywords":
            result.keywords = parseListSIMD(value)
          else:
            result.extraFields[key] = value
        elif currentAction.len > 0:
          result.actions[currentAction][key] = value

      lineStart = lineEnd + 1

  f.close()

  if not hasDesktopEntry:
    raise newException(DesktopEntryError, "File must start with [Desktop Entry] group")

  for key, found in foundRequiredKeys:
    if not found:
      raise newException(DesktopEntryError, "Required key missing: " & key)

  if result.`type` == "Application" and result.exec.len == 0:
    raise newException(DesktopEntryError, "Applications must have an Exec field")

when isMainModule:
  # Example usage and basic benchmark
  import times

  let start = cpuTime()
  var entry: DesktopEntry
  for i in 0 .. 1000:
    entry = parseDesktopEntry("/usr/share/applications/volctl.desktop")
  let duration = cpuTime() - start
  echo "Parsed 1000 times in ", duration, " seconds"
  echo entry[]
  echo entry.icon
