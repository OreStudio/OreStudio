/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TRADING_SCRIPTING_CHECK_HPP
#define ORES_TRADING_SCRIPTING_CHECK_HPP

#include "ores.trading.scripting/diagnostic.hpp"
#include "ores.trading.scripting/export.hpp"
#include "ores.trading.scripting/inputs.hpp"
#include "ores.trading.scripting/ore/ast.hpp"
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::scripting {

/**
 * @brief One variable a trade states for its script: a name, the type ORE reads
 * it as, and its value, or its values when it is a list.
 */
struct script_variable final {
    std::string name;
    std::string type;
    bool is_array = false;
    std::vector<std::string> values;
};

/**
 * @brief The findings of a parsed script against the variables a trade states.
 *
 * Every finding is reported together, ordered as diagnostics are: those located
 * in the text by line and column, then those about a variable by its name.
 *
 * - An input the trade does not state, a variable of the wrong type, and a list
 *   where one value belongs or the reverse, are errors about the variable
 *   (SCR2001, SCR2003, SCR2004); a variable the script never reads is a warning
 *   (SCR2002).
 * - A REQUIRE statement the trade's values break is an error at the statement
 *   (SCR3001). Only the statements every run reaches are evaluated, those
 *   outside any IF and loop, and only when every name one reads is a number the
 *   trade states, or the size of a list it states, and the script does not
 *   assign it; anything else is left to ORE. Equality is ORE's: equal within
 *   QuantLib's close_enough.
 * - A declared variable the script never uses is a warning at its declaration
 *   (SCR4001).
 *
 * @param tree the parsed script.
 * @param text the script's text, which locates each finding.
 * @param inputs the script's inputs; for a script with several variants, their
 * merge.
 * @param variables the variables the trade states; where two share a name, the
 * first is the one checked.
 */
ORES_TRADING_SCRIPTING_EXPORT std::vector<diagnostic>
check(const ASTNodePtr& tree,
      std::string_view text,
      std::span<const script_input> inputs,
      std::span<const script_variable> variables);

}

#endif
