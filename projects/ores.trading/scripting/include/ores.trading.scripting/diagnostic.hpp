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
#ifndef ORES_TRADING_SCRIPTING_DIAGNOSTIC_HPP
#define ORES_TRADING_SCRIPTING_DIAGNOSTIC_HPP

#include "ores.trading.scripting/export.hpp"
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>

namespace ores::trading::scripting {

/**
 * @brief Whether a finding prevents booking.
 */
enum class severity { error, warning };

/**
 * @brief A place in a script's text: a one-based line, and a one-based column
 * counted in Unicode code points, a tab counting as one, where it is known.
 */
struct position final {
    std::size_t line = 0;
    std::optional<std::size_t> column;
};

/**
 * @brief One finding of script validation, in the form a compiler reports one.
 *
 * A finding about the script's text is located by its start and, where known,
 * its end. A finding about a trade's variable states the variable as its
 * subject in place of a location.
 */
struct diagnostic final {
    scripting::severity severity = severity::error;
    std::string code;
    std::optional<position> start;
    std::optional<position> end;
    std::string message;
    std::optional<std::string> subject;
};

/**
 * @brief The meaning of one diagnostic code: its severity, a title and an
 * explanation. The codes are stated once, as data, in diagnostic_codes().
 */
struct diagnostic_code final {
    std::string_view code;
    scripting::severity severity;
    std::string_view title;
    std::string_view explanation;
};

/**
 * @brief Every diagnostic code, in code order.
 */
ORES_TRADING_SCRIPTING_EXPORT std::span<const diagnostic_code> diagnostic_codes();

/**
 * @brief The code's meaning, or nothing for a code no finding states.
 */
ORES_TRADING_SCRIPTING_EXPORT std::optional<diagnostic_code> code_of(std::string_view code);

/**
 * @brief The finding as =FILE:LINE:COLUMN: severity: message [code]=, the form
 * Emacs's compilation-mode reads; the column is left out where it is unknown,
 * and a finding with no location is reported against line one.
 */
ORES_TRADING_SCRIPTING_EXPORT std::string to_compiler_format(const diagnostic& d,
                                                             std::string_view file);

}

#endif
