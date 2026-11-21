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
 Copied into ORE Studio from ORE's OREData/ored/scripting/scriptparser.hpp at commit c3ffbae78 by
 projects/ores.trading/scripting/utility/import_ore_scripting.py. Do not edit:
 change the script, which states every change it makes.
*/

/*! \file ored/scripting/scriptparser.hpp
    \brief script parser
    \ingroup utilities
*/

#ifndef ORES_TRADING_SCRIPTING_ORE_SCRIPT_PARSER_HPP
#define ORES_TRADING_SCRIPTING_ORE_SCRIPT_PARSER_HPP

#include "ores.trading.scripting/ore/ast.hpp"
#include "ores.trading.scripting/ore/ore_compat.hpp"
#include <ostream>

namespace ores::trading::scripting {

struct ParserError {
    // always provided in case of an error
    std::string remainingInput;
    LocationInfo stoppedParsingAt;
    // only provided for a subset of possible errors
    std::string expectedWhat;
    LocationInfo expectedWhere;
    std::string scriptCurrentLine;
    std::string scriptContext;
    Size errorPos;
};

std::ostream& operator<<(std::ostream& out, const ParserError& error);

class ScriptParser {
public:
    ScriptParser(const std::string& script);
    // did the parsing succeed?
    bool success() const {
        return success_;
    }
    // null if not successful
    ASTNodePtr ast() const {
        return ast_;
    }
    // error info, if not successful
    const ParserError& error() const {
        return parserError_;
    }

private:
    bool success_;
    ASTNodePtr ast_;
    ParserError parserError_;
};

std::string printCodeContext(std::string script, const ASTNode* loc, bool compact = false);

}

#endif
