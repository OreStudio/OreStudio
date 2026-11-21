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
#ifndef ORES_TRADING_SCRIPTING_PARSE_HPP
#define ORES_TRADING_SCRIPTING_PARSE_HPP

#include "ores.trading.scripting/diagnostic.hpp"
#include "ores.trading.scripting/export.hpp"
#include "ores.trading.scripting/ore/ast.hpp"
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::scripting {

/**
 * @brief A script parsed by ORE's grammar: its syntax tree, or the diagnostics
 * of a script that does not parse.
 */
struct parse_result final {
    ASTNodePtr tree;
    std::vector<diagnostic> diagnostics;

    [[nodiscard]] bool parsed() const {
        return tree != nullptr;
    }
};

/**
 * @brief Parses a script with ORE's grammar.
 *
 * ORE's grammar stops at the first syntax error, so a script that does not
 * parse yields one diagnostic, located where ORE stopped. Where ORE's parser
 * raises an error of its own rather than stopping, the diagnostic states that
 * its position is unknown and carries no location.
 */
ORES_TRADING_SCRIPTING_EXPORT parse_result parse(std::string_view text);

/**
 * @brief The script's text, written back from its tree by ORE's converter.
 */
ORES_TRADING_SCRIPTING_EXPORT std::string ast_to_script(const ASTNodePtr& tree);

/**
 * @brief The tree's structure, one node per line, as ORE's printer writes it.
 */
ORES_TRADING_SCRIPTING_EXPORT std::string print_ast(const ASTNodePtr& tree,
                                                    bool with_locations = false);

/**
 * @brief A random tree, from ORE's generator, for tests that compare grammars.
 */
ORES_TRADING_SCRIPTING_EXPORT ASTNodePtr generate_random_ast(std::size_t max_sequence_length,
                                                             std::size_t max_depth,
                                                             std::size_t seed);

}

#endif
