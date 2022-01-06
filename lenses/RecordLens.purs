module Dependencies.RecordLens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_accum :: forall r a. Lens' { accum :: a | r } a
_accum = prop (Proxy :: Proxy "accum")

_arg1 :: forall r a. Lens' { arg1 :: a | r } a
_arg1 = prop (Proxy :: Proxy "arg1")

_arg2 :: forall r a. Lens' { arg2 :: a | r } a
_arg2 = prop (Proxy :: Proxy "arg2")

_arg3 :: forall r a. Lens' { arg3 :: a | r } a
_arg3 = prop (Proxy :: Proxy "arg3")

_arg4 :: forall r a. Lens' { arg4 :: a | r } a
_arg4 = prop (Proxy :: Proxy "arg4")

_arrow :: forall r a. Lens' { arrow :: a | r } a
_arrow = prop (Proxy :: Proxy "arrow")

_atime :: forall r a. Lens' { atime :: a | r } a
_atime = prop (Proxy :: Proxy "atime")

_autoClose :: forall r a. Lens' { autoClose :: a | r } a
_autoClose = prop (Proxy :: Proxy "autoClose")

_bar :: forall r a. Lens' { bar :: a | r } a
_bar = prop (Proxy :: Proxy "bar")

_binder :: forall r a. Lens' { binder :: a | r } a
_binder = prop (Proxy :: Proxy "binder")

_binders :: forall r a. Lens' { binders :: a | r } a
_binders = prop (Proxy :: Proxy "binders")

_bindings :: forall r a. Lens' { bindings :: a | r } a
_bindings = prop (Proxy :: Proxy "bindings")

_body :: forall r a. Lens' { body :: a | r } a
_body = prop (Proxy :: Proxy "body")

_branches :: forall r a. Lens' { branches :: a | r } a
_branches = prop (Proxy :: Proxy "branches")

_className :: forall r a. Lens' { className :: a | r } a
_className = prop (Proxy :: Proxy "className")

_close :: forall r a. Lens' { close :: a | r } a
_close = prop (Proxy :: Proxy "close")

_code :: forall r a. Lens' { code :: a | r } a
_code = prop (Proxy :: Proxy "code")

_column :: forall r a. Lens' { column :: a | r } a
_column = prop (Proxy :: Proxy "column")

_completed :: forall r a. Lens' { completed :: a | r } a
_completed = prop (Proxy :: Proxy "completed")

_cond :: forall r a. Lens' { cond :: a | r } a
_cond = prop (Proxy :: Proxy "cond")

_constraints :: forall r a. Lens' { constraints :: a | r } a
_constraints = prop (Proxy :: Proxy "constraints")

_consumed :: forall r a. Lens' { consumed :: a | r } a
_consumed = prop (Proxy :: Proxy "consumed")

_ctime :: forall r a. Lens' { ctime :: a | r } a
_ctime = prop (Proxy :: Proxy "ctime")

_cwd :: forall r a. Lens' { cwd :: a | r } a
_cwd = prop (Proxy :: Proxy "cwd")

_declarations :: forall r a. Lens' { declarations :: a | r } a
_declarations = prop (Proxy :: Proxy "declarations")

_decls :: forall r a. Lens' { decls :: a | r } a
_decls = prop (Proxy :: Proxy "decls")

_detached :: forall r a. Lens' { detached :: a | r } a
_detached = prop (Proxy :: Proxy "detached")

_dev :: forall r a. Lens' { dev :: a | r } a
_dev = prop (Proxy :: Proxy "dev")

_dot :: forall r a. Lens' { dot :: a | r } a
_dot = prop (Proxy :: Proxy "dot")

_dotAll :: forall r a. Lens' { dotAll :: a | r } a
_dotAll = prop (Proxy :: Proxy "dotAll")

_else :: forall r a. Lens' { else :: a | r } a
_else = prop (Proxy :: Proxy "else")

_encoding :: forall r a. Lens' { encoding :: a | r } a
_encoding = prop (Proxy :: Proxy "encoding")

_end :: forall r a. Lens' { end :: a | r } a
_end = prop (Proxy :: Proxy "end")

_env :: forall r a. Lens' { env :: a | r } a
_env = prop (Proxy :: Proxy "env")

_errno :: forall r a. Lens' { errno :: a | r } a
_errno = prop (Proxy :: Proxy "errno")

_error :: forall r a. Lens' { error :: a | r } a
_error = prop (Proxy :: Proxy "error")

_errors :: forall r a. Lens' { errors :: a | r } a
_errors = prop (Proxy :: Proxy "errors")

_exports :: forall r a. Lens' { exports :: a | r } a
_exports = prop (Proxy :: Proxy "exports")

_expr :: forall r a. Lens' { expr :: a | r } a
_expr = prop (Proxy :: Proxy "expr")

_failed :: forall r a. Lens' { failed :: a | r } a
_failed = prop (Proxy :: Proxy "failed")

_false :: forall r a. Lens' { false :: a | r } a
_false = prop (Proxy :: Proxy "false")

_fields :: forall r a. Lens' { fields :: a | r } a
_fields = prop (Proxy :: Proxy "fields")

_flags :: forall r a. Lens' { flags :: a | r } a
_flags = prop (Proxy :: Proxy "flags")

_fold :: forall r a. Lens' { fold :: a | r } a
_fold = prop (Proxy :: Proxy "fold")

_foldFull :: forall r a. Lens' { foldFull :: a | r } a
_foldFull = prop (Proxy :: Proxy "foldFull")

_formatError :: forall r a. Lens' { formatError :: a | r } a
_formatError = prop (Proxy :: Proxy "formatError")

_fundeps :: forall r a. Lens' { fundeps :: a | r } a
_fundeps = prop (Proxy :: Proxy "fundeps")

_gid :: forall r a. Lens' { gid :: a | r } a
_gid = prop (Proxy :: Proxy "gid")

_global :: forall r a. Lens' { global :: a | r } a
_global = prop (Proxy :: Proxy "global")

_guarded :: forall r a. Lens' { guarded :: a | r } a
_guarded = prop (Proxy :: Proxy "guarded")

_head :: forall r a. Lens' { head :: a | r } a
_head = prop (Proxy :: Proxy "head")

_header :: forall r a. Lens' { header :: a | r } a
_header = prop (Proxy :: Proxy "header")

_ignoreCase :: forall r a. Lens' { ignoreCase :: a | r } a
_ignoreCase = prop (Proxy :: Proxy "ignoreCase")

_importWrap :: forall r a. Lens' { importWrap :: a | r } a
_importWrap = prop (Proxy :: Proxy "importWrap")

_imports :: forall r a. Lens' { imports :: a | r } a
_imports = prop (Proxy :: Proxy "imports")

_importsFrom :: forall r a. Lens' { importsFrom :: a | r } a
_importsFrom = prop (Proxy :: Proxy "importsFrom")

_importsHiding :: forall r a. Lens' { importsHiding :: a | r } a
_importsHiding = prop (Proxy :: Proxy "importsHiding")

_importsHidingQualified :: forall r a. Lens' { importsHidingQualified :: a | r } a
_importsHidingQualified = prop (Proxy :: Proxy "importsHidingQualified")

_importsOpen :: forall r a. Lens' { importsOpen :: a | r } a
_importsOpen = prop (Proxy :: Proxy "importsOpen")

_importsQualified :: forall r a. Lens' { importsQualified :: a | r } a
_importsQualified = prop (Proxy :: Proxy "importsQualified")

_in :: forall r a. Lens' { in :: a | r } a
_in = prop (Proxy :: Proxy "in")

_indent :: forall r a. Lens' { indent :: a | r } a
_indent = prop (Proxy :: Proxy "indent")

_indentSpaces :: forall r a. Lens' { indentSpaces :: a | r } a
_indentSpaces = prop (Proxy :: Proxy "indentSpaces")

_indentUnit :: forall r a. Lens' { indentUnit :: a | r } a
_indentUnit = prop (Proxy :: Proxy "indentUnit")

_indentWidth :: forall r a. Lens' { indentWidth :: a | r } a
_indentWidth = prop (Proxy :: Proxy "indentWidth")

_index :: forall r a. Lens' { index :: a | r } a
_index = prop (Proxy :: Proxy "index")

_ino :: forall r a. Lens' { ino :: a | r } a
_ino = prop (Proxy :: Proxy "ino")

_input :: forall r a. Lens' { input :: a | r } a
_input = prop (Proxy :: Proxy "input")

_isBlockDevice :: forall r a. Lens' { isBlockDevice :: a | r } a
_isBlockDevice = prop (Proxy :: Proxy "isBlockDevice")

_isCharacterDevice :: forall r a. Lens' { isCharacterDevice :: a | r } a
_isCharacterDevice = prop (Proxy :: Proxy "isCharacterDevice")

_isDirectory :: forall r a. Lens' { isDirectory :: a | r } a
_isDirectory = prop (Proxy :: Proxy "isDirectory")

_isFIFO :: forall r a. Lens' { isFIFO :: a | r } a
_isFIFO = prop (Proxy :: Proxy "isFIFO")

_isFile :: forall r a. Lens' { isFile :: a | r } a
_isFile = prop (Proxy :: Proxy "isFile")

_isSocket :: forall r a. Lens' { isSocket :: a | r } a
_isSocket = prop (Proxy :: Proxy "isSocket")

_keyword :: forall r a. Lens' { keyword :: a | r } a
_keyword = prop (Proxy :: Proxy "keyword")

_killSignal :: forall r a. Lens' { killSignal :: a | r } a
_killSignal = prop (Proxy :: Proxy "killSignal")

_killed :: forall r a. Lens' { killed :: a | r } a
_killed = prop (Proxy :: Proxy "killed")

_label :: forall r a. Lens' { label :: a | r } a
_label = prop (Proxy :: Proxy "label")

_labels :: forall r a. Lens' { labels :: a | r } a
_labels = prop (Proxy :: Proxy "labels")

_leadingComments :: forall r a. Lens' { leadingComments :: a | r } a
_leadingComments = prop (Proxy :: Proxy "leadingComments")

_line :: forall r a. Lens' { line :: a | r } a
_line = prop (Proxy :: Proxy "line")

_lower :: forall r a. Lens' { lower :: a | r } a
_lower = prop (Proxy :: Proxy "lower")

_maxBuffer :: forall r a. Lens' { maxBuffer :: a | r } a
_maxBuffer = prop (Proxy :: Proxy "maxBuffer")

_mode :: forall r a. Lens' { mode :: a | r } a
_mode = prop (Proxy :: Proxy "mode")

_module :: forall r a. Lens' { module :: a | r } a
_module = prop (Proxy :: Proxy "module")

_mtime :: forall r a. Lens' { mtime :: a | r } a
_mtime = prop (Proxy :: Proxy "mtime")

_multiline :: forall r a. Lens' { multiline :: a | r } a
_multiline = prop (Proxy :: Proxy "multiline")

_name :: forall r a. Lens' { name :: a | r } a
_name = prop (Proxy :: Proxy "name")

_names :: forall r a. Lens' { names :: a | r } a
_names = prop (Proxy :: Proxy "names")

_nextIndent :: forall r a. Lens' { nextIndent :: a | r } a
_nextIndent = prop (Proxy :: Proxy "nextIndent")

_nlink :: forall r a. Lens' { nlink :: a | r } a
_nlink = prop (Proxy :: Proxy "nlink")

_of :: forall r a. Lens' { of :: a | r } a
_of = prop (Proxy :: Proxy "of")

_open :: forall r a. Lens' { open :: a | r } a
_open = prop (Proxy :: Proxy "open")

_operator :: forall r a. Lens' { operator :: a | r } a
_operator = prop (Proxy :: Proxy "operator")

_operators :: forall r a. Lens' { operators :: a | r } a
_operators = prop (Proxy :: Proxy "operators")

_pageWidth :: forall r a. Lens' { pageWidth :: a | r } a
_pageWidth = prop (Proxy :: Proxy "pageWidth")

_path :: forall r a. Lens' { path :: a | r } a
_path = prop (Proxy :: Proxy "path")

_patterns :: forall r a. Lens' { patterns :: a | r } a
_patterns = prop (Proxy :: Proxy "patterns")

_perms :: forall r a. Lens' { perms :: a | r } a
_perms = prop (Proxy :: Proxy "perms")

_position :: forall r a. Lens' { position :: a | r } a
_position = prop (Proxy :: Proxy "position")

_prec :: forall r a. Lens' { prec :: a | r } a
_prec = prop (Proxy :: Proxy "prec")

_qualified :: forall r a. Lens' { qualified :: a | r } a
_qualified = prop (Proxy :: Proxy "qualified")

_range :: forall r a. Lens' { range :: a | r } a
_range = prop (Proxy :: Proxy "range")

_rdev :: forall r a. Lens' { rdev :: a | r } a
_rdev = prop (Proxy :: Proxy "rdev")

_result :: forall r a. Lens' { result :: a | r } a
_result = prop (Proxy :: Proxy "result")

_ribbonRatio :: forall r a. Lens' { ribbonRatio :: a | r } a
_ribbonRatio = prop (Proxy :: Proxy "ribbonRatio")

_ribbonWidth :: forall r a. Lens' { ribbonWidth :: a | r } a
_ribbonWidth = prop (Proxy :: Proxy "ribbonWidth")

_separator :: forall r a. Lens' { separator :: a | r } a
_separator = prop (Proxy :: Proxy "separator")

_shell :: forall r a. Lens' { shell :: a | r } a
_shell = prop (Proxy :: Proxy "shell")

_size :: forall r a. Lens' { size :: a | r } a
_size = prop (Proxy :: Proxy "size")

_start :: forall r a. Lens' { start :: a | r } a
_start = prop (Proxy :: Proxy "start")

_statements :: forall r a. Lens' { statements :: a | r } a
_statements = prop (Proxy :: Proxy "statements")

_stderr :: forall r a. Lens' { stderr :: a | r } a
_stderr = prop (Proxy :: Proxy "stderr")

_stdio :: forall r a. Lens' { stdio :: a | r } a
_stdio = prop (Proxy :: Proxy "stdio")

_stdout :: forall r a. Lens' { stdout :: a | r } a
_stdout = prop (Proxy :: Proxy "stdout")

_sticky :: forall r a. Lens' { sticky :: a | r } a
_sticky = prop (Proxy :: Proxy "sticky")

_stream :: forall r a. Lens' { stream :: a | r } a
_stream = prop (Proxy :: Proxy "stream")

_super :: forall r a. Lens' { super :: a | r } a
_super = prop (Proxy :: Proxy "super")

_symbol :: forall r a. Lens' { symbol :: a | r } a
_symbol = prop (Proxy :: Proxy "symbol")

_syscall :: forall r a. Lens' { syscall :: a | r } a
_syscall = prop (Proxy :: Proxy "syscall")

_tail :: forall r a. Lens' { tail :: a | r } a
_tail = prop (Proxy :: Proxy "tail")

_then :: forall r a. Lens' { then :: a | r } a
_then = prop (Proxy :: Proxy "then")

_timeout :: forall r a. Lens' { timeout :: a | r } a
_timeout = prop (Proxy :: Proxy "timeout")

_title :: forall r a. Lens' { title :: a | r } a
_title = prop (Proxy :: Proxy "title")

_token :: forall r a. Lens' { token :: a | r } a
_token = prop (Proxy :: Proxy "token")

_trailingComments :: forall r a. Lens' { trailingComments :: a | r } a
_trailingComments = prop (Proxy :: Proxy "trailingComments")

_true :: forall r a. Lens' { true :: a | r } a
_true = prop (Proxy :: Proxy "true")

_typeArrowPlacement :: forall r a. Lens' { typeArrowPlacement :: a | r } a
_typeArrowPlacement = prop (Proxy :: Proxy "typeArrowPlacement")

_types :: forall r a. Lens' { types :: a | r } a
_types = prop (Proxy :: Proxy "types")

_uid :: forall r a. Lens' { uid :: a | r } a
_uid = prop (Proxy :: Proxy "uid")

_unicode :: forall r a. Lens' { unicode :: a | r } a
_unicode = prop (Proxy :: Proxy "unicode")

_upper :: forall r a. Lens' { upper :: a | r } a
_upper = prop (Proxy :: Proxy "upper")

_value :: forall r a. Lens' { value :: a | r } a
_value = prop (Proxy :: Proxy "value")

_vars :: forall r a. Lens' { vars :: a | r } a
_vars = prop (Proxy :: Proxy "vars")

_where :: forall r a. Lens' { where :: a | r } a
_where = prop (Proxy :: Proxy "where")
