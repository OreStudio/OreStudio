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
#ifndef ORES_TRADING_SCRIPTING_INPUTS_HPP
#define ORES_TRADING_SCRIPTING_INPUTS_HPP

#include "ores.trading.scripting/export.hpp"
#include "ores.trading.scripting/ore/ast.hpp"
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::scripting {

/**
 * @brief One input of a script: a variable the script reads and neither
 * declares nor assigns, whose value the trade supplies.
 *
 * The type is the one ORE's scripted trade states for the variable: Number,
 * Event, Index, Currency or Daycounter. The rule, where the script states one,
 * is the condition of the first REQUIRE statement whose first name is the
 * input's, with its whitespace collapsed.
 */
struct script_input final {
    std::string name;
    std::string type;
    bool is_array = false;
    std::optional<std::string> rule;

    bool operator==(const script_input&) const = default;
};

/**
 * @brief The inputs of one variant of a script, in the order the script first
 * uses them.
 *
 * The type of an input follows from its use: an index called with a date is an
 * Index, a date argument of PAY is an Event, its currency a Currency, the first
 * argument of dcf a Daycounter. An input is an array when the script indexes it
 * or asks its SIZE. TODAY is the run's date, not an input.
 *
 * @param tree the parsed variant.
 * @param text the variant's text, from which each rule is read.
 */
ORES_TRADING_SCRIPTING_EXPORT std::vector<script_input> inputs_of(const ASTNodePtr& tree,
                                                                  std::string_view text);

/**
 * @brief The inputs of a script with several variants: every input any variant
 * reads, the first variant's first, since a trade states what each variant it
 * may run reads. An input keeps its first type; it takes its rule from the
 * first variant that states one.
 */
ORES_TRADING_SCRIPTING_EXPORT std::vector<script_input>
merge_inputs(std::span<const std::vector<script_input>> variants);

}

#endif
