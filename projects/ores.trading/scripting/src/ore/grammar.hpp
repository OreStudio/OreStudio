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
 Copied into ORE Studio from ORE's OREData/ored/scripting/grammar.hpp at commit c3ffbae78 by
 projects/ores.trading/scripting/utility/import_ore_scripting.py. Do not edit:
 change the script, which states every change it makes.
*/

/*! \file ored/scripting/grammar.hpp
    \brief payoff script grammar
    \ingroup utilities
*/

#ifndef ORES_TRADING_SCRIPTING_SRC_ORE_GRAMMAR_HPP
#define ORES_TRADING_SCRIPTING_SRC_ORE_GRAMMAR_HPP

#include "ores.trading.scripting/ore/ast.hpp"
#include "ores.trading.scripting/ore/ore_compat.hpp"
#include <boost/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_line_pos_iterator.hpp>
#include <stack>
#include <string>
#include <vector>

namespace ores::trading::scripting {

namespace qi = boost::spirit::qi;
namespace phoenix = boost::phoenix;

using ScriptGrammarIterator = boost::spirit::line_pos_iterator<std::string::const_iterator>;

// ast node annotator

struct ASTNodeAnnotation {
    ASTNodeAnnotation(std::stack<ASTNodePtr>& evalStack, const ScriptGrammarIterator first)
        : evalStack_(evalStack)
        , first_(first) {}
    void operator()(const ScriptGrammarIterator f, const ScriptGrammarIterator l) const;
    std::stack<ASTNodePtr>& evalStack_;
    const ScriptGrammarIterator first_;
};

// grammar

struct ScriptGrammar : public qi::grammar<ScriptGrammarIterator, qi::space_type> {
    ScriptGrammar(ScriptGrammarIterator first);
    bool hasError;
    ScriptGrammarIterator errorBegin, errorPos, errorEnd;
    boost::spirit::info errorWhat;
    std::stack<ASTNodePtr> evalStack;

private:
    qi::rule<ScriptGrammarIterator, std::string(), qi::space_type> keyword, varname;
    qi::rule<ScriptGrammarIterator, qi::space_type> varexpr, instructionseq, instruction,
        declaration, ifthenelse, loop, assignment, require, sort, permute, condition, condition2,
        condition3, term, product, factor;
    ASTNodeAnnotation annotate;
};

}

#endif
