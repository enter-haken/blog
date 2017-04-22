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

# documenting a larger project

In a more complex examples you have a deep inheritance hierarchy.
When you are coming into a mature project / product you'll find interfaces and/or abstract classes defining base behavior or base data structures.
The goal is, to reuse documentation as much as possible.

For demonstration there is a data base class `PocoBase`, which defines a unique `Id` for all data entities used in an application.

    using System;
    
    namespace SandcastleTest.Generic.POCO
    {
        /// <summary>
        /// Base class for all POCOs
        /// </summary>
        public abstract class PocoBase
        {
            private Guid _Id = Guid.Empty;
    
            /// <summary>
            /// Id for the entity
            /// </summary>
            /// <remarks>
            /// If the <see cref="_Id"/> is <see cref="Guid.Empty"/> 
            /// a new <see cref="Guid"/> is generated.
            /// </remarks>
            public Guid Id
            {
                get
                {
                    if (_Id == Guid.Empty)
                        _Id = Guid.NewGuid();
    
                    return _Id;
                }
                set
                {
                    _Id = value;
                }
            }
    
            /// <summary>
            /// Gets a hascode for a <see cref="PocoBase"/>
            /// </summary>
            /// <returns> a hascode for a <see cref="PocoBase"/></returns>
            public override int GetHashCode()
            {
                return Id.GetHashCode();
            }
        }
    }

The documentation part will often be a bigger part of the code. 
You can fold the documentation in Visual Studio if you like. 
Most editors offer this feature, do get a more compact view on the code if necessary.

When a data access layer uses this pocos based on `PocoBase` you can define some base methods for data access.

    using SandcastleTest.Generic.POCO;
    
    using System;
    using System.Collections.Generic;
    
    namespace SandcastleTest.Generic.DAL
    {
        /// <summary>
        /// A base interface for all CRUD operations
        /// </summary>
        /// <typeparam name="T"><inheritdoc cref="PocoBase" select="summary"/></typeparam>
        public interface ICreateReadUpdateDelete<T> where T : PocoBase
        {
            /// <summary>
            /// Create a new <paramref name="entity"/>
            /// </summary>
            /// <param name="entity"><inheritdoc cref="PocoBase" select="summary"/></param>
            void Create(T entity);
    
            /// <summary>
            /// Get a list of <typeparamref name="T"/>
            /// </summary>
            /// <returns>a list of <typeparamref name="T"/></returns>
            List<T> GetList();
    
            /// <summary>
            /// Get an entity by <see cref="PocoBase.Id"/>
            /// </summary>
            /// <returns>an entity of type <typeparamref name="T"/></returns>
            T GetEntity(Guid id);
    
            /// <summary>
            /// Update an entity of <typeparamref name="T"/>
            /// </summary>
            /// <param name="entity"><inheritdoc cref="PocoBase" select="summary"/></param>
            /// <returns>if the update succeeded, this method returns true, otherwise false.</returns>
            bool Update(T entity);
    
            /// <summary>
            /// Deletes an entity of  <typeparamref name="T"/>
            /// </summary>
            /// <param name="entity"><inheritdoc cref="PocoBase" select="summary"/></param>
            /// <returns>if the deletion succeeded, this method returns true, otherwise false.</returns>
            bool Delete(T entity);
        }
    }
 
The `summary` documentation of `PocoBase` is reused with the `<inheritdoc cref="PocoBase" select="summary"/>` statement. 
The interface contains a base documentation for the generic behavior of managing CRUD operations.

The next abstraction layer is a implementation of these methods.
The simplest example uses the file system as a storage.

    using Newtonsoft.Json;
    
    using SandcastleTest.Generic.POCO;
    
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    
    namespace SandcastleTest.Generic.DAL.FileSystemStorage
    {
        /// <inheritdoc/>
        /// <summary>
        /// Handling CRUD actions for file system storage.
        /// </summary>
        public abstract class Crud<T> : ICreateReadUpdateDelete<T>
            where T : PocoBase
        {
            /// <summary>
            /// Gets the file name of the current entity.
            /// The filename is constructed with the poco class name and the extension .json
            /// </summary>
            private string FileName => $"{typeof(T).Name }.json";
    
            /// <summary>
            /// Get all contents of the file, containing all the contents of the entity.
            /// </summary>
            private string Content => File.ReadAllText(FileName);
    
            /// <summary>
            /// Saves a raw string representation to file system.
            /// </summary>
            /// <param name="rawContent">a raw string representation</param>
            private void Store(string rawContent) => File.WriteAllText(FileName, rawContent);
    
            /// <summary>
            /// Stores a <see cref="List{T}"/> to  the file system
            /// </summary>
            /// <param name="allEntities">all entities</param>
            private void Store(List<T> allEntities) => Store(JsonConvert.SerializeObject(allEntities));
    
            /// <inheritdoc/>
            /// <exception cref="ArgumentNullException">is thrown, when <paramref name="entity"/> is null</exception>
            public void Create(T entity)
            {
                if (entity == null)
                    throw new ArgumentNullException(nameof(entity));
    
                var allEntities = GetList();
                if (Exists(allEntities, entity))
                    return; // no update on creation
    
                allEntities.Add(entity);
    
                Store(allEntities);
            }
    
            /// <inheritdoc/>
            public bool Delete(T entity)
            {
                var allEntities = GetList();
                if (!(Exists(allEntities, entity)))
                    return false ;
    
                var itemToRemove = allEntities.SingleOrDefault(x => x.Id == entity.Id);
                if (itemToRemove == null)
                    return false;
    
                if (allEntities.Remove(itemToRemove))
                {
                    Store(allEntities);
                    return true;
                }
    
                return false;
            }
    
            /// <inheritdoc/>
            public T GetEntity(Guid id) => GetList().SingleOrDefault(x => x.Id == id);
    
            /// <inheritdoc/>
            public List<T> GetList()
            {
                try
                {
                    return JsonConvert.DeserializeObject<List<T>>(Content);
                }
                catch
                {
                    // maybe file does not exists...
                    return new List<T>();
                }
            }
    
            /// <inheritdoc/>
            public bool Update(T entity)
            {
                if (entity == null)
                    throw new ArgumentNullException(nameof(entity));
    
                var allEntities = GetList();
                if (Exists(allEntities, entity))
                {
                    allEntities.Replace(entity);
                    Store(allEntities);
    
                    return true;
                }
                return false;
            }
    
            /// <summary>
            /// Checks, if <paramref name="allEntities" /> contains a <paramref name="entityToCheck"/>
            /// </summary>
            /// <param name="allEntities">all entities</param>
            /// <param name="entityToCheck">an entity to check</param>
            /// <returns>true, if <paramref name="allEntities" /> contains a <paramref name="entityToCheck"/></returns>
            private bool Exists(List<T> allEntities, T entityToCheck)
            {
                if (allEntities == null)
                    return false;
    
                return allEntities.Contains(entityToCheck, new PocoBaseEqualityComparer());
            }
        }
    }

This code should just work. 
I know this is not performance friendly. 
For a demonstration it is just enough.

As you can see in the [generated result][crudDoc], a more concrete class can inherit a part of the documentation of their base class and interface. 
You must try to write the base documentation as reusable as possible. 

If you like to store a `Customer` on file system, the classes can look like following.

First a kind of `Person` is needed. 
It can be assumed, that in a bigger application this class is a base class for human like entities (e.g. Customer, Employee, Manager ...).

    namespace SandcastleTest.Generic.POCO
    {
        /// <summary>
        /// A base class with base properties for a person.
        /// </summary>
        public abstract class Person :  PocoBase
        {
            /// <summary>
            /// First name of a person
            /// </summary>
            public string FirstName { get; set; }
    
            /// <summary>
            /// Last name of a person
            /// </summary>
            public string LastName { get; set; }
    
            /// <summary>
            /// Overrides a <see cref="ToString"/> representation of an person
            /// </summary>
            /// <returns>a <see cref="ToString"/> representation of an person</returns>
            public override string ToString()
            {
                return $"Person: {FirstName} - {LastName}";
            }
        }
    }

Deriving from `PocoBase` gives every `Person` an identifier.
The [documentation][personDoc] for `FistName` and `LastName` can be inherited from deriving classes.

    namespace SandcastleTest.Generic.POCO
    {
        /// <summary>
        /// A customer
        /// </summary>
        public class Customer : Person
        {
            /// <summary>
            /// A customer number
            /// </summary>
            public string CustomerNumber { get; set; }
    
            /// <summary>
            /// Overrides the <see cref="ToString"/> method
            /// </summary>
            /// <returns>a string representation of a customer</returns>
            public override string ToString()
            {
                return $"Customer: {CustomerNumber} ({base.ToString()})";
            }
        }
    }

For this example the `Customer` has a `CustomerNumber`. 
Customer related stuff can be added here, if needed.
Every thing else is derived from the base class `Person`.
The [result][customerDoc] looks promising.

The simplest class for doing CRUD operation on a `Customer` for a file system is just a empty subclass of `Crud<T>`.

    using SandcastleTest.Generic.POCO;

    namespace SandcastleTest.Generic.DAL.FileSystemStorage
    {
        /// <summary>
        /// Provides methods to access a <see cref="Customer"/>
        /// </summary>
        public class CustomerAccess : Crud<Customer>
        {
        }
    }

The interesting thing here is the [generated documentation][customerAccessDoc] for the above class.
You can see, that most method documentation is inherited from base classes and interfaces.

You can explore more from the [test code][sandcastleTestCode], or you can browse through the generated [result][sandcastleTestHelp].





[sandcastleTestCode]: https://github.com/enter-haken/SandcastleDemo/tree/master/SandcastleTest
[sandcastleTestHelp]: /example/sandcastle/index.html
[release]:  https://github.com/EWSoftware/SHFB/releases
[literateProgramming]: https://en.wikipedia.org/wiki/Literate_programming
[doxygen]: http://www.stack.nl/~dimitri/doxygen/
[sandcastle]: https://github.com/EWSoftware/SHFB
[sandcastleDownload]: https://github.com/EWSoftware/SHFB/releases
[crudDoc]: /example/sandcastle/html/e8807ce7-b71c-ccbc-c71e-15c9299f5d9b.htm
[personDoc]: /example/sandcastle/html/cc5624ed-c08d-ab34-c42e-4b97de71fa4c.htm
[customerDoc]: /example/sandcastle/html/7696b3d3-c681-f48b-bda3-db48fcaecb00.htm
[customerAccessDoc]: /example/sandcastle/html/f9bf04a0-9eaa-8366-d929-d5814c9b97cd.htm
