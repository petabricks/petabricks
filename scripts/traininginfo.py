#!/usr/bin/python
import re
from xml.dom.minidom import parse
nameof = lambda t: str(t.getAttribute("name"))

class XmlDictApi:
  def __init__(self, xml):
    self.xml = xml
    self.overrides=dict()
  def __getitem__(self, k):
    if self.overrides.has_key(k):
      return self.overrides[k]
    else:
      return str(self.xml.getAttribute(k))
  def __setitem__(self, k, v):
    self.overrides[k]=v

class TrainingInfo:
  def __init__(self, infoxml, transforms=None):
    self.instances = [self]
    if type(infoxml) is type(""):
      self.infoxml = parse(infoxml)
    else:
      self.infoxml = infoxml
    if transforms is not None:
      self.transforms = transforms
    else:
      self.transforms = dict()
    for t in self.infoxml.getElementsByTagName("transform"):
      v=TrainingInfo(t, self.transforms)
      self.transforms[nameof(t)]=v
      if t.getAttribute("templateChoice")=="0":
        self.transforms[t.getAttribute("templateName")] = v
    #fill in instances
    for t in self.infoxml.getElementsByTagName("transform"):
      if t.getAttribute("templateChoice") not in ("0","-1"):
        self.transforms[t.getAttribute("templateName")].instances.append(self.transforms[nameof(t)])

  def accuracyTarget(self):
    return float(self.infoxml.getAttribute("accuracyTarget"))
  
  def isVariableAccuracy(self):
    return int(self.infoxml.getAttribute("isVariableAccuracy"))>0

  def name(self):
    return nameof(self.infoxml)

  def transform(self, name):
    return self.transforms[name]

  def globalsec(self):
    return TrainingInfo(self.infoxml.getElementsByTagName("global")[0])

  def algchoices(self):
    rex = re.compile("_([0-9]+)$")
    rv = []
    seen = set()
    for t in self.infoxml.getElementsByTagName("algchoice"):
      if nameof(t) in seen:
        continue
      else:
        seen.add(nameof(t))
      rv.append(XmlDictApi(t))
      n = int(rex.search(nameof(t)).group(1))
      rv[-1]['number'] = n
    return rv

  def tunables(self):
    return map(XmlDictApi, self.infoxml.getElementsByTagName("tunable"))

  def tunablesDict(self):
    tunables = dict()
    for t in self.tunables():
      tunables[t['name']] = t
    return tunables

  def rulesInAlgchoice(self, number):
    matches = filter(lambda x: x['number']==number, self.algchoices())
    assert len(matches)==1
    return range(int(matches[0]['rules']))

  def calls(self):
    rv=set()
    for t in self.infoxml.getElementsByTagName("calls"):
      rv.add(str(t.getAttribute('callee')))
    return map(self.transform, rv)


  

