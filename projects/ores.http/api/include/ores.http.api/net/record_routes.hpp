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
#ifndef ORES_HTTP_API_NET_RECORD_ROUTES_HPP
#define ORES_HTTP_API_NET_RECORD_ROUTES_HPP

#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/export.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/lexical_cast.hpp>
#include <exception>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>

/**
 * @brief What every record route a screen reads shares.
 *
 * The screens read each resource through the same addresses, and each
 * generated unit refuses, reads and answers in the same way. The parts that
 * need neither the unit's types nor a transport live here once.
 */
namespace ores::http::net::record_routes {

/**
 * @brief A refusal a route states, with the status and the code the browser reads.
 *
 * It is not a standard exception, so a handler's catch of a standard exception
 * cannot take it for a failure of its own.
 */
struct ORES_HTTP_API_EXPORT refusal final {
    domain::http_status status;
    std::string code;
    std::string message;
};

/**
 * @brief The sentence the browser sees when a route cannot answer.
 *
 * The detail goes to the log, so the browser sees no library's own words.
 */
inline constexpr std::string_view unanswered = "The service could not answer this request.";

/**
 * @brief Refuse with this status, code and message.
 */
[[noreturn]] ORES_HTTP_API_EXPORT void
refuse(domain::http_status status, std::string code, std::string message);

/**
 * @brief Refuse as a request the browser stated wrongly.
 */
[[noreturn]] ORES_HTTP_API_EXPORT void invalid(std::string message);

/**
 * @brief The response a refusal states.
 */
ORES_HTTP_API_EXPORT domain::http_response answer(const refusal& r);

/**
 * @brief A write's answer: no content.
 */
ORES_HTTP_API_EXPORT domain::http_response no_content();

/**
 * @brief The text without its leading and trailing spaces, tabs and line ends.
 */
ORES_HTTP_API_EXPORT std::string trimmed(const std::string& raw);

/**
 * @brief A code named in the address: 1 to 100 characters once trimmed.
 *
 * @throws refusal naming the sentence when the code is empty or too long.
 */
ORES_HTTP_API_EXPORT std::string code(const std::string& raw, const std::string& sentence);

/**
 * @brief Whether a reason is 1 to 200 characters once trimmed.
 */
ORES_HTTP_API_EXPORT bool has_reason(const std::string& reason);

/**
 * @brief Refuse with the status a refused outcome names.
 *
 * A conflict the service states no words for says what it means.
 */
ORES_HTTP_API_EXPORT void require_ok(const ores::utility::domain::result& result);

/**
 * @brief One parameter read as the member's own type, refused by name when malformed.
 */
template <typename T>
T read(const std::string& raw, const std::string& name) {
    try {
        if constexpr (std::is_same_v<T, std::string>) {
            return raw;
        } else if constexpr (std::is_same_v<T, bool>) {
            if (raw == "true")
                return true;
            if (raw == "false")
                return false;
            throw std::invalid_argument(raw);
        } else {
            return boost::lexical_cast<T>(raw);
        }
    } catch (const std::exception&) {
        invalid("Invalid value for " + name + ": " + raw);
    }
}

}

#endif
