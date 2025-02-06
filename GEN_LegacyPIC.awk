BEGIN {
FS="."
}
{
cmd= "getchipdata "substr($1, 2)
system ( cmd )

}