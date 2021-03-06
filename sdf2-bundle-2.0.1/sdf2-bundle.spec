Summary: The Syntax Definition Formalism SDF2
Name: sdf2-bundle
Version: 2.0.1
Release: 5183
License: LGPL
Group: Development/Languages
URL: http://www.cwi.nl/htbin/sen1/twiki/bin/view/SEN1/SDF2
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-buildroot
Requires: aterm >= 2.0
Provides:  %{name} = %{version}

%description

SDF2 is a syntax definition formalism with the following features:

    * Modular syntax definition (parametrized modules, symbol renaming)
    * Integrated lexical and context-free syntax
    * Declarative disambiguation constructs (priorities, associativity, and more)
    * Regular expression shorthands
    * All non-circular context-free grammars allowed!

SDF2 is implemented in two parts:

    * The ParsetableGenerator
    * SGLR: a scannerless generalized LR parser

The above implementation accepts arbitrary context-free grammars as input.

%prep
%setup -q

%build
CFLAGS="-D__NO_CTYPE" ./configure --prefix=%{_prefix}  --with-aterm=%{_prefix}
make

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,-)
%{_bindir}/
%{_includedir}
#/%{name}/
%{_libdir}/
%{_prefix}/man/
%{_datadir}/
%doc


%changelog
* Thu Jan  2 2003 Eelco Visser <visser@cs.uu.nl> 1.5-1
- Initial build.