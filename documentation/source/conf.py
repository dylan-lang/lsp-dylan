# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
sys.path.insert(0, os.path.abspath('../../_packages/sphinx-extensions/current/src/sphinxcontrib'))
sys.path.insert(0, os.path.abspath('../../../_packages/sphinx-extensions/current/src/sphinxcontrib'))
import dylan.themes as dylan_themes


# -- Project information -----------------------------------------------------

project = 'lsp-dylan'
copyright = '2019, Peter Hull'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'dylan.domain'
]

# Necessary to make things like '.. current-module:: foo' work.
primary_domain = 'dylan'

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'furo'
# html_theme = dylan_themes.get_html_theme_default()

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']


# Ignore GitHub source code links with line number anchors in them because
# they're apparently handled via Javascript and won't be found in the HTML.
# https://www.sphinx-doc.org/en/master/usage/configuration.html#confval-linkcheck_anchors_ignore
linkcheck_anchors_ignore = [r"^L\d+$"]

# Ignore certificate verification
tls_verify = False
