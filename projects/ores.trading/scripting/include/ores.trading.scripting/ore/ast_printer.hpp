/*
 Copyright (C) 2019 Quaternion Risk Management Ltd
 All rights reserved.

 This file is part of ORE, a free-software/open-source library
 for transparent pricing and risk analysis - http://opensourcerisk.org

 ORE is free software: you can redistribute it and/or modify it
 under the terms of the Modified BSD License.  You should have received a
 copy of the license along with this program.
 The license is also available online at <http://opensourcerisk.org>

 This program is distributed on the basis that it will form a useful
 contribution to risk analytics and model standardisation, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the license for more details.
*/

/*
 Copied into ORE Studio from ORE's OREData/ored/scripting/astprinter.hpp at commit c3ffbae78 by
 projects/ores.trading/scripting/utility/import_ore_scripting.py. Do not edit:
 change the script, which states every change it makes.
*/

/*! \file ored/scripting/astprinter.hpp
    \brief ast printer
    \ingroup utilities
*/

#ifndef ORES_TRADING_SCRIPTING_ORE_AST_PRINTER_HPP
#define ORES_TRADING_SCRIPTING_ORE_AST_PRINTER_HPP

#include "ores.trading.scripting/ore/ast.hpp"
#include "ores.trading.scripting/ore/ore_compat.hpp"

namespace ores::trading::scripting {

std::string to_string(const ASTNodePtr root, const bool printLocationInfo = true);

}

#endif
