import os
from setuptools import find_packages, setup

name = 'mcl'
version = '0.1.0.dev'
description = 'A programming language for the measurement calculus'

long_description = open('README.md', encoding='utf-8').read()

reqs = open('requirements.txt').readlines()
requirements = [r.strip() for r in reqs]

packs = (['mcl'])

setup(
  name=name,
  version=version,
  url='http://github.com/seunomonije/mclang',
  install_requires=requirements,
  description=description,
  long_description=long_description,
  packages=packs,
  package_data={'mcl': ['mcl/*']},
)