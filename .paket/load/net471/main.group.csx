namespace PaketLoadScripts

#r "C:\\Users\\maxw\\.nuget\\packages\\fable.import.browser\\1.3.0\\lib\\netstandard1.6\\Fable.Import.Browser.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fable.import.hmr\\0.1.0\\lib\\netstandard1.6\\Fable.Import.HMR.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\dotnet.projinfo\\0.20.0\\lib\\net45\\Dotnet.ProjInfo.Helpers.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fsharp.core\\4.5.2\\lib\\net45\\FSharp.Core.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.net.http\\4.3.4\\lib\\net46\\System.Net.Http.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.security.cryptography.x509certificates\\4.3.2\\lib\\net461\\System.Security.Cryptography.X509Certificates.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.security.cryptography.cng\\4.5.0\\lib\\net47\\System.Security.Cryptography.Cng.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.security.cryptography.csp\\4.3.0\\lib\\net46\\System.Security.Cryptography.Csp.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.security.cryptography.openssl\\4.5.0\\lib\\netstandard2.0\\System.Security.Cryptography.OpenSsl.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.security.cryptography.algorithms\\4.3.1\\lib\\net463\\System.Security.Cryptography.Algorithms.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.linq.expressions\\4.3.0\\lib\\net463\\System.Linq.Expressions.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.security.cryptography.encoding\\4.3.0\\lib\\net46\\System.Security.Cryptography.Encoding.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.diagnostics.process\\4.3.0\\lib\\net461\\System.Diagnostics.Process.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.diagnostics.tracesource\\4.3.0\\lib\\net46\\System.Diagnostics.TraceSource.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.globalization.extensions\\4.3.0\\lib\\net46\\System.Globalization.Extensions.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.linq\\4.3.0\\lib\\net463\\System.Linq.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.security.cryptography.primitives\\4.3.0\\lib\\net46\\System.Security.Cryptography.Primitives.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.text.regularexpressions\\4.3.0\\lib\\net463\\System.Text.RegularExpressions.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.diagnostics.diagnosticsource\\4.5.1\\lib\\net46\\System.Diagnostics.DiagnosticSource.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.runtime.interopservices\\4.3.0\\lib\\net463\\System.Runtime.InteropServices.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.console\\4.3.1\\lib\\net46\\System.Console.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.io.filesystem\\4.3.0\\lib\\net46\\System.IO.FileSystem.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.reflection\\4.3.0\\lib\\net462\\System.Reflection.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\microsoft.win32.registry\\4.5.0\\lib\\net461\\Microsoft.Win32.Registry.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.globalization.calendars\\4.3.0\\lib\\net46\\System.Globalization.Calendars.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.io\\4.3.0\\lib\\net462\\System.IO.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.threading.threadpool\\4.3.0\\lib\\net46\\System.Threading.ThreadPool.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\microsoft.win32.primitives\\4.3.0\\lib\\net46\\Microsoft.Win32.Primitives.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.diagnostics.tracing\\4.3.0\\lib\\net462\\System.Diagnostics.Tracing.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.io.filesystem.primitives\\4.3.0\\lib\\net46\\System.IO.FileSystem.Primitives.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.runtime.extensions\\4.3.0\\lib\\net462\\System.Runtime.Extensions.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.security.accesscontrol\\4.5.0\\lib\\net461\\System.Security.AccessControl.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.threading.thread\\4.3.0\\lib\\net46\\System.Threading.Thread.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.memory\\4.5.1\\lib\\netstandard2.0\\System.Memory.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.runtime\\4.3.0\\lib\\net462\\System.Runtime.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.security.principal.windows\\4.5.1\\lib\\net461\\System.Security.Principal.Windows.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\newtonsoft.json\\11.0.2\\lib\\net45\\Newtonsoft.Json.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.collections.immutable\\1.5.0\\lib\\netstandard2.0\\System.Collections.Immutable.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.reflection.typeextensions\\4.5.1\\lib\\net461\\System.Reflection.TypeExtensions.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.runtime.compilerservices.unsafe\\4.5.2\\lib\\netstandard2.0\\System.Runtime.CompilerServices.Unsafe.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fable.elmish\\2.0.3\\lib\\netstandard2.0\\Fable.Elmish.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fable.powerpack\\2.0.2\\lib\\netstandard2.0\\Fable.PowerPack.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fable.react\\4.1.3\\lib\\netstandard2.0\\Fable.React.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\thoth.json\\2.5.0\\lib\\netstandard2.0\\Thoth.Json.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\dotnet.projinfo\\0.20.0\\lib\\net45\\Dotnet.ProjInfo.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fable.core\\2.0.1\\lib\\netstandard2.0\\Fable.Core.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\system.reflection.metadata\\1.6.0\\lib\\netstandard2.0\\System.Reflection.Metadata.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fable.elmish.browser\\2.1.0\\lib\\netstandard2.0\\Fable.Elmish.Browser.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fable.elmish.debugger\\2.0.2\\lib\\netstandard2.0\\Fable.Elmish.Debugger.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fable.elmish.react\\2.0.0\\lib\\netstandard2.0\\Fable.Elmish.React.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fsharp.compiler.service\\25.0.1\\lib\\net45\\FSharp.Compiler.Service.dll" 
#r "C:\\Users\\maxw\\.nuget\\packages\\fable.elmish.hmr\\2.1.0\\lib\\netstandard2.0\\Fable.Elmish.HMR.dll" 
#r "mscorlib" 
#r "System" 
#r "System.ComponentModel.Composition" 
#r "System.Core" 
#r "System.Numerics" 
#r "System.Net.Http" 
#r "ISymWrapper, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a" 
#r "System.IO" 
#r "System.Runtime" 