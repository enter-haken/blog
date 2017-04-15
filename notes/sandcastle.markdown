---
title: sandcastle
---

Donald Knuth wrote about [literate programming][literateProgramming] years ago.
Literate programming is about keeping the documentation near to the implementation.
Over years, several products like [doxygen][doxygen] are helping to solve this kind of problem.
There exists a product named [sandcastle][sandcastle] for .NET based code.
It is able to generate MSDN like documentation.

<!--more-->

At first you have to download the [latest][sandcastleDownload] version of sandcastle.
After creating a `.shfbproj` file, the `XML documentation file` check box in the `output` section of the project build configuration has to be enabled.

# convenience batch files

For convenience you can add some batch files to the solution root.

## make debug build

    REM --
    REM -- This script builds the solution for the debug configuration
    REM --
    
    call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\Tools\VsDevCmd.bat"
    msbuild SandCastleTest.sln /p:Configuration=Debug
    
    IF "%1" == "--noPause" goto END
    
    pause
    
    :END

## remove generated files

    REM --
    REM -- This script deletes all obj / bin folders from solution
    REM --
    
    FOR /F "tokens=*" %%G IN ('DIR /B /AD /S obj') DO RMDIR /S /Q "%%G"
    FOR /F "tokens=*" %%G IN ('DIR /B /AD /S bin') DO RMDIR /S /Q "%%G"
    
    REM -- delete generated folders from solution root directory
    RMDIR /S /Q Help

## generate help files and start local http server

    @ECHO OFF
    REM --
    REM -- build documentation
    REM --
    
    CALL deepClean.bat
    CALL make_debugBuild.bat --noPause
    
    REM -- build documentation
    CALL "C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\Tools\VsDevCmd.bat"
    msbuild SandCastleTestDocumentation.shfbproj
    
    REM -- open browser first ...
    START "" http://localhost:8000
    REM -- ... because the web server is blocking
    
    cd Help
    
    REM -- the next command creates a local web server and serves 
    REM -- recursively all contents from the current folder.
    
    REM -- this works, if python >3 is installed, 
    REM -- and the path variable is up to date
    python -m http.server

It is assumed, that python > 3.x is installed. 
You can choose an other web server, if you like.

# Start

At the beginning there are a lot of warnings in the `Error List`. 
These are reminder for missing comments.
When the documentation is build at this point, it will be very empty.
Now is the time fill some contents.

Lets start with a simple [example][sandcastleTestBase].

A printable document is described by an interface.

    namespace SandcastleTest.Base
    {
        /// <summary>
        /// A printable document
        /// </summary>
        public interface IPrintable
        {
            /// <summary>
            /// Prints a <see cref="IPrintable"/>
            /// </summary>
            void Print();
        }
    }

The `<see>` tag references a type within a documentation.
If the `cref` attribute value is misspelled, you get a compiler warning. 

If you apply the `IPrintable` interface to a `Document`,

using System;
using System.Text;

    namespace SandcastleTest.Base
    {
        /// <inheritdoc/>
        /// <summary>
        /// A document
        /// </summary>
        public class Document : IPrintable
        {
            /// <summary>
            /// Documents title. 
            /// </summary>
            public string Title { get; set; }
    
            /// <summary>
            /// Content of a document
            /// </summary>
            public string Content { get; set; }
    
            /// <inheritdoc/>
            public void Print()
            {
                Console.WriteLine(ToString());
            }
    
            /// <summary>
            /// Get a string representation of a <see cref="Document"/>
            /// </summary>
            /// <returns>the documents <see cref="Title"/> and the <see cref="Content"/></returns>
            public override string ToString()
            {
                var sb = new StringBuilder();
                sb.AppendLine(Title);
                sb.AppendLine(Content);
    
                return sb.ToString();
            }
        }
    }

you can see, the interface method `Print()` does not has to be documented.
The documentation is inherited from the interface.
The `<inheritdoc>` tag is used for this application.

For namespace documentation a `NamespaceDoc` class has to be added to the namespace.

using System.Runtime.CompilerServices;

    namespace SandcastleTest.Base
    {
        /// <summary>
        /// this comment appears in the namespace documentation
        /// </summary>
        [CompilerGenerated]
        class NamespaceDoc
        {
            // this class is only used for namespace documentation
        }
    }

The `CompilerGenerated` attribute prevents sandcastle to generate a documentation for the class itself.

For this [code][sandcastleTestCode] you get this [result][sandcastleTestHelp].



[sandcastleTestCode]: https://github.com/enter-haken/SandcastleDemo/tree/master/SandcastleTest
[sandcastleTestHelp]: /example/sandcastle/index.html
[release]:  https://github.com/EWSoftware/SHFB/releases
[literateProgramming]: https://en.wikipedia.org/wiki/Literate_programming
[doxygen]: http://www.stack.nl/~dimitri/doxygen/
[sandcastle]: https://github.com/EWSoftware/SHFB
[sandcastleDownload]: https://github.com/EWSoftware/SHFB/releases
