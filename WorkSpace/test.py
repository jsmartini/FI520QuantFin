# this file tests to make sure the library is built correctly and is
# visible to the python3 interpreter

import QLib

print(QLib.ver())
print(QLib.Bonds.bondF("2010-1-15", "2000-1-1", "2026-1-1", 2))
print(QLib.Bonds.bondNRemaining("2010-1-15", "2000-1-1", "2026-1-1", 2))