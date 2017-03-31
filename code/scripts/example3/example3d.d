extern (C) nothrow @nogc @safe pure:

/* Template to generate string */
template GenWrap(string type)
{
	enum string GenWrap = type ~ " " ~ type[0] ~ "mult(" ~ type ~ " x, " ~ type ~ " y)\n{\n\treturn x*y;\n}";
}

mixin(GenWrap!"double");
mixin(GenWrap!"float");