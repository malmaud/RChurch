#!/usr/bin/python
import re
from pprint import pprint

source_filenames = [
    "../include/standard-preamble.church",
    "../church/church-eval/desugar.ss",
    "../church/standard-env.ss"
]

doc_mapping = {
    "define" : "Compound Procedures",
    "register-sugar!" : "Syntactic Sugar",
    "register-primitive-erp!" : "Elementary Random Procedures",
    "register-primitive-procedure!" : "Primitive Procedures",
    "register-primitive-constant!" : "Primitive Constants",
    "register-query!" : "Queries"
}

doc_pattern = """
    (;;?\s*\@.+?)                       # capture the documentation comments
    \(
    (define|                            # capture what type of documentation this is
     register-primitive-procedure!|
     register-primitive-erp!|
     register-primitive-constant!|
     register-query!|
     register-sugar!)
     [\s\n]+
     (.+?)                              # capture the name of the object that is documented
     \)
     """

header_template = """
== %(header)s ==
"""

group_template = """
%(entries)s
"""

entry_template = """
=== <tt>%(form)s</tt> ===
* %(desc)s
%(params)s* Returns: %(return)s"""

param_template = """* <tt>%(param)s</tt>: %(value)s"""


def startswith(s, start):
    """
    Returns True if string s starts with string start, False otherwise.
    """
    return s[:len(start)] == start

def remainder(s, start):
    """
    Assumes that string s starts with string start. Returns everything that follows
    in s after start.
    """
    return s[len(start):]

def parse_doclines(doclines):
    """
    Takes a list of documentation comment lines and
    parses them into a dictionary.
    """
    doc = {}
    multiline = False
    for dl in doclines:
        if dl == "@desc":
            doc["desc"] = ""
            multiline = "desc"
        elif startswith(dl, "@form"):
            doc["form"] = remainder(dl, "@form ")
            multiline = False
        elif startswith(dl, "@param"):
            doc.setdefault("param", [])
            dl_parts = re.split("\s+", dl, maxsplit=2)
            if len(dl_parts) == 2:
                dl_parts.append("")
            elif len(dl_parts) == 3 and len(dl_parts[2]) > 2 and dl_parts[2][:3] == "...":
                dl_parts[1] = dl_parts[1] + " ..."
                dl_parts[2] = dl_parts[2][4:]
            doc["param"].append( (dl_parts[1], dl_parts[2]) )
            multiline = False            
        elif startswith(dl, "@return"):            
            doc["return"] = remainder(dl, "@return ")
            multiline = False
        elif multiline:
            doc["desc"] += dl + " "
        else:
            raise Exception, "Error! Don't know what to do with doc %s" % dl.__repr__()
    return doc

def add_doc(source, docs=None):
    """
    Takes a source code file with comments and returns a dictionary
    that contains all documentation in this file, sorted by type
    (e.g. compound, primitive, ...).
    """
    if not docs:
        docs = dict([(doc_type, []) for doc_type in doc_mapping.values()])
    for find in re.finditer(doc_pattern, source, re.DOTALL+re.VERBOSE):
        condition = find.group(2)
        doclines = [line.strip(" ;\t\n") for line in re.split("\n\s*;+\s*", find.group(1))]
        doc = parse_doclines(doclines)
        if not doc.get("form"):
            if condition == "define":
                doc["form"] = "(%s)" % find.group(3)[1:]
            else:
                operator = find.group(3).split(" ")[0][1:].strip()
                operands = " ".join([p[0] for p in doc.get("param", [])])
                if operands == "":
                    doc["form"] = "(%s)" % operator
                else:
                    doc["form"] = "(%s %s)" % (operator, operands)
        docs[doc_mapping[condition]].append(doc)
    return docs

def sources_to_doc(sources):
    """
    Takes a list of source code strings and returns a documentation dictionary,
    sorted by type (e.g. compound, primitive, ...).
    """
    doc = None
    for source in sources:
        doc = add_doc(source, doc)
    # pprint(doc)
    return doc

def doc_to_wiki(doc):
    """
    Takes a documentation dictionary and returns Mediawiki output.
    """
    wikiout = []
    for (section, section_docs) in doc.items():
        section_docs = sorted(section_docs, key=lambda x: x.get("form", "..."))
        wikiout.append(header_template % { "header" : section })
        entries = []
        for section_doc in section_docs:
            param_out = []
            for (param, value) in section_doc.get("param", []):
                param_data = {
                    "param" : param,
                    "value" : value
                }
                param_out.append(param_template % param_data)
            if param_out != []:
                param_out.append("")
            entry_data = {
                "form" : section_doc.get("form", "..."),
                "desc" : section_doc.get("desc", "(no description)"),
                "params" : "\n".join(param_out),
                "return" : section_doc.get("return", "(no return value)")
            }
            entries.append(entry_template % entry_data)
        wikiout.append(group_template % { "entries" : "\n".join(entries)})        
    return "\n".join(wikiout)


if __name__ == "__main__":
    sources = [open(fn).read() for fn in source_filenames]
    doc = sources_to_doc(sources)
    print doc_to_wiki(doc)
