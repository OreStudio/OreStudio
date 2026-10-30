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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_UTILITY_DOMAIN_PROTOCOL_CODEC_HPP
#define ORES_UTILITY_DOMAIN_PROTOCOL_CODEC_HPP

#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <array>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::utility::codec {

/**
 * @brief The envelope's enums travel by the name of the enumerator.
 *
 * reflect-cpp writes an enum as the word the enumerator is spelled with, in
 * both formats, so a value is the same word here. The names are spelled out
 * rather than derived from the enumerator: a rename is then a change to the
 * wire, which is what it is, instead of a silent one.
 */
template <>
struct text_form<ores::utility::domain::outcome> {
    static constexpr std::string_view wire_name = "an outcome";

    static constexpr std::array<std::string_view, 7> names{
        "ok", "invalid", "denied", "missing", "conflict", "unavailable", "failed"};

    static std::string to_text(const ores::utility::domain::outcome& v) {
        return std::string(names.at(static_cast<std::size_t>(v)));
    }

    static ores::utility::domain::outcome from_text(std::string_view text) {
        for (std::size_t i = 0; i < names.size(); ++i)
            if (names[i] == text)
                return static_cast<ores::utility::domain::outcome>(i);
        throw std::invalid_argument("Not an outcome: " + std::string(text));
    }
};

template <>
struct text_form<ores::utility::domain::precondition_kind> {
    static constexpr std::string_view wire_name = "a precondition";

    static constexpr std::array<std::string_view, 3> names{
        "any", "must_not_exist", "must_match_version"};

    static std::string to_text(const ores::utility::domain::precondition_kind& v) {
        return std::string(names.at(static_cast<std::size_t>(v)));
    }

    static ores::utility::domain::precondition_kind from_text(std::string_view text) {
        for (std::size_t i = 0; i < names.size(); ++i)
            if (names[i] == text)
                return static_cast<ores::utility::domain::precondition_kind>(i);
        throw std::invalid_argument("Not a precondition: " + std::string(text));
    }
};

template <>
struct text_form<ores::utility::domain::scope> {
    static constexpr std::string_view wire_name = "a scope";

    static constexpr std::array<std::string_view, 2> names{"direct", "subtree"};

    static std::string to_text(const ores::utility::domain::scope& v) {
        return std::string(names.at(static_cast<std::size_t>(v)));
    }

    static ores::utility::domain::scope from_text(std::string_view text) {
        for (std::size_t i = 0; i < names.size(); ++i)
            if (names[i] == text)
                return static_cast<ores::utility::domain::scope>(i);
        throw std::invalid_argument("Not a scope: " + std::string(text));
    }
};

}

/**
 * @brief The envelope's shapes, stated where its structs are declared.
 *
 * These are hand-written because the envelope is: a message holds them, so the
 * carriers recurse into them exactly as they recurse into a generated message.
 */
namespace ores::utility::domain {

[[nodiscard]] constexpr auto wire_fields(const field_failure&) {
    return std::tuple{ores::utility::codec::field{"field", &field_failure::field},
                      ores::utility::codec::field{"code", &field_failure::code},
                      ores::utility::codec::field{"message", &field_failure::message}};
}

[[nodiscard]] constexpr auto wire_fields(const result&) {
    return std::tuple{ores::utility::codec::field{"outcome", &result::outcome},
                      ores::utility::codec::field{"code", &result::code},
                      ores::utility::codec::field{"message", &result::message},
                      ores::utility::codec::field{"fields", &result::fields}};
}

[[nodiscard]] constexpr auto wire_fields(const precondition&) {
    return std::tuple{ores::utility::codec::field{"kind", &precondition::kind},
                      ores::utility::codec::field{"version", &precondition::version}};
}

[[nodiscard]] constexpr auto wire_fields(const change_intent&) {
    return std::tuple{ores::utility::codec::field{"reason_code", &change_intent::reason_code},
                      ores::utility::codec::field{"commentary", &change_intent::commentary}};
}

[[nodiscard]] constexpr auto wire_fields(const order&) {
    return std::tuple{ores::utility::codec::field{"field", &order::field},
                      ores::utility::codec::field{"descending", &order::descending}};
}

}

#endif
