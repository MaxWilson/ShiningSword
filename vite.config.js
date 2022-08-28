import { defineConfig } from 'vite';
import path from 'path';
import envCompatible from 'vite-plugin-env-compatible';
import { createHtmlPlugin } from 'vite-plugin-html';
import { viteCommonjs } from '@originjs/vite-plugin-commonjs';

// https://vitejs.dev/config/
export default defineConfig({
  resolve: {
    alias: [
      {
        find: '@',
        replacement: path.resolve(__dirname,'src')
      },
      {
        find: 'core-js/es6',
        replacement: path.resolve(__dirname,'core-js/es')
      }
    ],
    extensions: [
      '.mjs',
      '.js',
      '.ts',
      '.jsx',
      '.tsx',
      '.json',
      '.vue'
    ]
  },
  plugins: [
    viteCommonjs(),
    envCompatible(),
    createHtmlPlugin({
      minify: 'auto',
      inject: {
        tags: [],
        data: {
          title: 'Webpack App'
        }
      }
    })
  ],
  base: 'auto',
  mode: 'production',
  build: {
    rollupOptions: {
      input: {
        app0: 'C:\code\ShiningSword\src\ShiningSword\UI\App.fs.js',
        dev0: 'C:\code\ShiningSword\src\ShiningSword\Dev\App.fs.js'
      },
      output: {
        entryFileNames: '[name].[hash].js'
      }
    },
    outDir: path.resolve(__dirname, 'dist')
  },
  server: {
    strictPort: false,
    port: 8080,
    proxy: {
      '/api/*': {
        target: 'http://localhost:5000',
        changeOrigin: true
      }
    },
    base: path.resolve(__dirname, 'src/ShiningSword/public')
  },
  define: {
    'process.env.ALLUSERSPROFILE': '"C:\\ProgramData"',
    'process.env.APPDATA': '"C:\\Users\\maxw\\AppData\\Roaming"',
    'process.env.ChocolateyInstall': '"C:\\ProgramData\\chocolatey"',
    'process.env.ChocolateyLastPathUpdate': '"132984000967196134"',
    'process.env.CommandPromptType': '"Native"',
    'process.env.CommonProgramFiles': '"C:\\Program Files\\Common Files"',
    'process.env.CommonProgramFiles(x86)': '"C:\\Program Files (x86)\\Common Files"',
    'process.env.CommonProgramW6432': '"C:\\Program Files\\Common Files"',
    'process.env.COMPUTERNAME': '"MSI"',
    'process.env.ComSpec': '"C:\\WINDOWS\\system32\\cmd.exe"',
    'process.env.configsetroot': '"C:\\WINDOWS\\ConfigSetRoot"',
    'process.env.DevEnvDir': '"C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\"',
    'process.env.DriverData': '"C:\\Windows\\System32\\Drivers\\DriverData"',
    'process.env.ExtensionSdkDir': '"C:\\Program Files (x86)\\Microsoft SDKs\\Windows Kits\\10\\ExtensionSDKs"',
    'process.env.EXTERNAL_INCLUDE': '"C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.19041.0\\ucrt;C:\\Program Files (x86)\\Windows Kits\\10\\\\include\\10.0.19041.0\\\\um;C:\\Program Files (x86)\\Windows Kits\\10\\\\include\\10.0.19041.0\\\\shared;C:\\Program Files (x86)\\Windows Kits\\10\\\\include\\10.0.19041.0\\\\winrt;C:\\Program Files (x86)\\Windows Kits\\10\\\\include\\10.0.19041.0\\\\cppwinrt;C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.8\\include\\um"',
    'process.env.FPS_BROWSER_APP_PROFILE_STRING': '"Internet Explorer"',
    'process.env.FPS_BROWSER_USER_PROFILE_STRING': '"Default"',
    'process.env.Framework40Version': '"v4.0"',
    'process.env.FrameworkDir': '"C:\\Windows\\Microsoft.NET\\Framework\\"',
    'process.env.FrameworkDir32': '"C:\\Windows\\Microsoft.NET\\Framework\\"',
    'process.env.FrameworkVersion': '"v4.0.30319"',
    'process.env.FrameworkVersion32': '"v4.0.30319"',
    'process.env.FSHARPINSTALLDIR': '"C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\CommonExtensions\\Microsoft\\FSharp\\Tools"',
    'process.env.HOMEDRIVE': '"C:"',
    'process.env.HOMEPATH': '"\\Users\\maxw"',
    'process.env.INCLUDE': '"C:\\Program Files (x86)\\Windows Kits\\10\\include\\10.0.19041.0\\ucrt;C:\\Program Files (x86)\\Windows Kits\\10\\\\include\\10.0.19041.0\\\\um;C:\\Program Files (x86)\\Windows Kits\\10\\\\include\\10.0.19041.0\\\\shared;C:\\Program Files (x86)\\Windows Kits\\10\\\\include\\10.0.19041.0\\\\winrt;C:\\Program Files (x86)\\Windows Kits\\10\\\\include\\10.0.19041.0\\\\cppwinrt;C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.8\\include\\um"',
    'process.env.LIB': '"C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.8\\lib\\um\\x86;C:\\Program Files (x86)\\Windows Kits\\10\\lib\\10.0.19041.0\\ucrt\\x86;C:\\Program Files (x86)\\Windows Kits\\10\\\\lib\\10.0.19041.0\\\\um\\x86"',
    'process.env.LIBPATH': '"C:\\Program Files (x86)\\Windows Kits\\10\\UnionMetadata\\10.0.19041.0;C:\\Program Files (x86)\\Windows Kits\\10\\References\\10.0.19041.0;C:\\Windows\\Microsoft.NET\\Framework\\v4.0.30319"',
    'process.env.LOCALAPPDATA': '"C:\\Users\\maxw\\AppData\\Local"',
    'process.env.LOGONSERVER': '"\\\\MSI"',
    'process.env.NETFXSDKDir': '"C:\\Program Files (x86)\\Windows Kits\\NETFXSDK\\4.8\\"',
    'process.env.NUMBER_OF_PROCESSORS': '"12"',
    'process.env.OneDrive': '"C:\\Users\\maxw\\OneDrive - Microsoft"',
    'process.env.OneDriveCommercial': '"C:\\Users\\maxw\\OneDrive - Microsoft"',
    'process.env.OS': '"Windows_NT"',
    'process.env.Path': '"C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\VC\\VCPackages;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\CommonExtensions\\Microsoft\\TestWindow;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\CommonExtensions\\Microsoft\\TeamFoundation\\Team Explorer;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\MSBuild\\Current\\bin\\Roslyn;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Team Tools\\Performance Tools;C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v10.0A\\bin\\NETFX 4.8 Tools\\;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\CommonExtensions\\Microsoft\\FSharp\\Tools;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\Extensions\\Microsoft\\CodeCoverage.Console;C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.19041.0\\\\x86;C:\\Program Files (x86)\\Windows Kits\\10\\bin\\\\x86;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\\\MSBuild\\Current\\Bin\\amd64;C:\\Windows\\Microsoft.NET\\Framework\\v4.0.30319;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\Tools\\;C:\\Program Files (x86)\\Microsoft SDKs\\Azure\\CLI2\\wbin;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Windows\\System32\\OpenSSH\\;C:\\Program Files (x86)\\NVIDIA Corporation\\PhysX\\Common;C:\\Program Files\\NVIDIA Corporation\\NVIDIA NvDLISR;C:\\WINDOWS\\system32;C:\\WINDOWS;C:\\WINDOWS\\System32\\Wbem;C:\\WINDOWS\\System32\\WindowsPowerShell\\v1.0\\;C:\\WINDOWS\\System32\\OpenSSH\\;C:\\usr\\bin\\nodejs\\;C:\\Program Files\\dotnet\\;c:\\usr\\bin\\git\\cmd;C:\\ProgramData\\chocolatey\\bin;C:\\Program Files (x86)\\Yarn\\bin\\;C:\\Users\\maxw\\AppData\\Local\\Microsoft\\WindowsApps;C:\\Users\\maxw\\AppData\\Roaming\\npm;C:\\Users\\maxw\\.dotnet\\tools;C:\\usr\\bin\\auto;c:\\usr\\bin\\fiddler;C:\\Users\\maxw\\AppData\\Local\\Yarn\\bin;C:\\Users\\maxw\\.dotnet\\tools;C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\VC\\Linux\\bin\\ConnectionManagerExe"',
    'process.env.PATHEXT': '".COM;.EXE;.BAT;.CMD;.VBS;.VBE;.JS;.JSE;.WSF;.WSH;.MSC"',
    'process.env.PROCESSOR_ARCHITECTURE': '"AMD64"',
    'process.env.PROCESSOR_IDENTIFIER': '"Intel64 Family 6 Model 165 Stepping 2, GenuineIntel"',
    'process.env.PROCESSOR_LEVEL': '"6"',
    'process.env.PROCESSOR_REVISION': '"a502"',
    'process.env.ProgramData': '"C:\\ProgramData"',
    'process.env.ProgramFiles': '"C:\\Program Files"',
    'process.env.ProgramFiles(x86)': '"C:\\Program Files (x86)"',
    'process.env.ProgramW6432': '"C:\\Program Files"',
    'process.env.PROMPT': '"$P$G"',
    'process.env.PSModulePath': '"C:\\Program Files\\WindowsPowerShell\\Modules;C:\\WINDOWS\\system32\\WindowsPowerShell\\v1.0\\Modules;C:\\Program Files (x86)\\Microsoft Azure Information Protection\\Powershell"',
    'process.env.PUBLIC': '"C:\\Users\\Public"',
    'process.env.SESSIONNAME': '"Console"',
    'process.env.SystemDrive': '"C:"',
    'process.env.SystemRoot': '"C:\\WINDOWS"',
    'process.env.TEMP': '"C:\\Users\\maxw\\AppData\\Local\\Temp"',
    'process.env.TMP': '"C:\\Users\\maxw\\AppData\\Local\\Temp"',
    'process.env.UATDATA': '"C:\\WINDOWS\\CCM\\UATData\\D9F8C395-CAB8-491d-B8AC-179A1FE1BE77"',
    'process.env.UCRTVersion': '"10.0.19041.0"',
    'process.env.UniversalCRTSdkDir': '"C:\\Program Files (x86)\\Windows Kits\\10\\"',
    'process.env.USERDNSDOMAIN': '"redmond.corp.microsoft.com"',
    'process.env.USERDOMAIN': '"REDMOND"',
    'process.env.USERDOMAIN_ROAMINGPROFILE': '"REDMOND"',
    'process.env.USERNAME': '"maxw"',
    'process.env.USERPROFILE': '"C:\\Users\\maxw"',
    'process.env.VCIDEInstallDir': '"C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\IDE\\VC\\"',
    'process.env.VCINSTALLDIR': '"C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\VC\\"',
    'process.env.VisualStudioVersion': '"17.0"',
    'process.env.VS170COMNTOOLS': '"C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\Common7\\Tools\\"',
    'process.env.VSCMD_ARG_app_plat': '"Desktop"',
    'process.env.VSCMD_ARG_HOST_ARCH': '"x86"',
    'process.env.VSCMD_ARG_TGT_ARCH': '"x86"',
    'process.env.VSCMD_VER': '"17.3.0"',
    'process.env.VSINSTALLDIR': '"C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\"',
    'process.env.windir': '"C:\\WINDOWS"',
    'process.env.WindowsLibPath': '"C:\\Program Files (x86)\\Windows Kits\\10\\UnionMetadata\\10.0.19041.0;C:\\Program Files (x86)\\Windows Kits\\10\\References\\10.0.19041.0"',
    'process.env.WindowsSdkBinPath': '"C:\\Program Files (x86)\\Windows Kits\\10\\bin\\"',
    'process.env.WindowsSdkDir': '"C:\\Program Files (x86)\\Windows Kits\\10\\"',
    'process.env.WindowsSDKLibVersion': '"10.0.19041.0\\"',
    'process.env.WindowsSdkVerBinPath': '"C:\\Program Files (x86)\\Windows Kits\\10\\bin\\10.0.19041.0\\"',
    'process.env.WindowsSDKVersion': '"10.0.19041.0\\"',
    'process.env.WindowsSDK_ExecutablePath_x64': '"C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v10.0A\\bin\\NETFX 4.8 Tools\\x64\\"',
    'process.env.WindowsSDK_ExecutablePath_x86': '"C:\\Program Files (x86)\\Microsoft SDKs\\Windows\\v10.0A\\bin\\NETFX 4.8 Tools\\"',
    'process.env.__DOTNET_ADD_32BIT': '"1"',
    'process.env.__DOTNET_PREFERRED_BITNESS': '"32"',
    'process.env.__VSCMD_PREINIT_PATH': '"C:\\Program Files (x86)\\Microsoft SDKs\\Azure\\CLI2\\wbin;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Windows\\System32\\OpenSSH\\;C:\\Program Files (x86)\\NVIDIA Corporation\\PhysX\\Common;C:\\Program Files\\NVIDIA Corporation\\NVIDIA NvDLISR;C:\\WINDOWS\\system32;C:\\WINDOWS;C:\\WINDOWS\\System32\\Wbem;C:\\WINDOWS\\System32\\WindowsPowerShell\\v1.0\\;C:\\WINDOWS\\System32\\OpenSSH\\;C:\\usr\\bin\\nodejs\\;C:\\Program Files\\dotnet\\;c:\\usr\\bin\\git\\cmd;C:\\ProgramData\\chocolatey\\bin;C:\\Program Files (x86)\\Yarn\\bin\\;C:\\Users\\maxw\\AppData\\Local\\Microsoft\\WindowsApps;C:\\Users\\maxw\\AppData\\Roaming\\npm;C:\\Users\\maxw\\.dotnet\\tools;C:\\usr\\bin\\auto;c:\\usr\\bin\\fiddler;C:\\Users\\maxw\\AppData\\Local\\Yarn\\bin;C:\\Users\\maxw\\.dotnet\\tools"',
    'process.env': '{}'
  }
})
