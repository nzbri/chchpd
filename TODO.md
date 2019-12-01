# TODO

- We need to implement anonymous IDs properly. This means that all tables need to return a session ID column that incorporates the anonymous ID. This probably means that we should, at package load, read in from the participant data a lookup table containing a mapping of study IDs to anonymous IDs. We should probably at the same time read in a table of birth dates. This would avoid importing the participant table yet again in the import_sessions() function, which is currently needed to calculate the age at each session.
- Probably need to ask Reza about how to do this, i.e. storing it in some sort of hidden environment variable.
- This initial import would also be a useful test. If we can't read it at package load, then give some sort of useful error message.
- The anonymous import should really be a package-level option, so that all functions can access it, rather than it being passed as a parameter. How is this done?

# HOWTO

## Tag a release:
git tag -a v0.2.1 -m "Some description of the release"
git push --follow-tags

