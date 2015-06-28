Pywikibot
=========

The Pywikibot framework is a Python library that interfaces with the
`MediaWiki API <https://www.mediawiki.org/wiki/Special:MyLanguage/API:Main_page>`_
version 1.14 or higher.

Also included are various general function scripts that can be adapted for
different tasks.

Quick start
-----------

::

    git clone https://gerrit.wikimedia.org/r/pywikibot/core.git
    cd core
    git submodule update --init
    python pwb.py script_name

Our `installation
guide <https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:Pywikibot/Installation>`_
has more details for advanced usage.

Basic Usage
-----------

If you wish to write your own script it's very easy to get started:

::

    import pywikibot
    site = pywikibot.Site('en', 'wikipedia')  # The site we want to run our bot on
    page = pywikibot.Page(site, 'Wikipedia:Sandbox')
    page.text = page.text.replace('foo', 'bar')
    page.save('Replacing "foo" with "bar"')  # Saves the page


The contents of the package
----------------------------

    +----------------------------------------------------------------------------------+
    |    README and config files:                                                      |
    +===========================+======================================================+
    |    CREDITS                | List of major contributors to this module            |
    +---------------------------+------------------------------------------------------+
    |    ChangeLog              | Log file to keep track of major changes versionwise  |
    +---------------------------+------------------------------------------------------+
    |    ez_setup.py            | Bootstrap distribute installation file, can also be  |
    |                           | run to install or upgrade setuptools.                |
    +---------------------------+------------------------------------------------------+
    |    generate_family_file.py| Creates a new family file.                           |
    +---------------------------+------------------------------------------------------+
    |    generate_user_files.py | Creates user-config.py or user-fixes.py              |
    +---------------------------+------------------------------------------------------+
    |    LICENSE                | a reference to the MIT license                       |
    +---------------------------+------------------------------------------------------+
    |    pwb.py                 | Wrapper script to use Pywikibot in 'directory' mode  |
    +---------------------------+------------------------------------------------------+
    |    README-conversion.txt  | Guide to converting bot scripts from version 1       |
    |                           | of the Pywikibot framework to version 2              |
    +---------------------------+------------------------------------------------------+
    |    README.rst             | Short info string used by Pywikibot Nightlies        |
    +---------------------------+------------------------------------------------------+
    |    requirements.txt       | PIP requirements file                                |
    +---------------------------+------------------------------------------------------+
    |    setup.py               | Installer script for Pywikibot 2.0 framework         |
    +---------------------------+------------------------------------------------------+
    |    user-config.py.sample  | Example user-config.py file for reference            |
    +---------------------------+------------------------------------------------------+

    +----------------------------------------------------------------------------------+
    |    Directories                                                                   |
    +===========================+======================================================+
    |    externals              | Contains all external software that might be used by |
    |                           | by Pywikibot scripts and libraries.                  |
    +---------------------------+------------------------------------------------------+
    |    logs                   | Contains logfiles                                    |
    +---------------------------+------------------------------------------------------+
    |    pywikibot              | Contains some libraries and control files            |
    +---------------------------+------------------------------------------------------+
    |    scripts                | Contains all bots and utility scripts                |
    +---------------------------+------------------------------------------------------+
    |    tests                  | Some test stuff for the developing team              |
    +---------------------------+------------------------------------------------------+


Contributing
------------

Our code is maintained on Wikimedia's `Gerrit installation <https://gerrit.wikimedia.org/>`_,
`learn <https://www.mediawiki.org/wiki/Special:MyLanguage/Developer_access>`_ how to get
started.

.. image:: https://secure.travis-ci.org/wikimedia/pywikibot-core.png?branch=master
   :alt: Build Status
   :target: https://travis-ci.org/wikimedia/pywikibot-core
