# dec sh vars
 shared variables using C-Style structs

# here's a fun example
```
dec_shvars.Register("ClanData",{
"string", "name",
"string", "tag",
"color", "tagColor"
}, true)
```

## Didn't Falco make something like this that you could just use?
yes but afaik it's only for individual variables, do you expect me to encode multiple variables in like a string or something?

### haven't you done that though?
more often than you'd want to hear about
