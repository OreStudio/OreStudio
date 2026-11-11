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
#ifndef ORES_HTTP_API_NET_CLASSIFICATION_ROUTES_HPP
#define ORES_HTTP_API_NET_CLASSIFICATION_ROUTES_HPP

#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

/**
 * @brief What every classification list route shares.
 *
 * The classification screen maintains each code list through the same routes,
 * and every list's rows have a code and a description, with a name, a display
 * order or both. The bodies the screen sends, the row the screen reads and the
 * write a row becomes are therefore the same for every list, and live here
 * once. A list whose rows lack a name or an order simply has no such member.
 */
namespace ores::http::net::classification_routes {

/**
 * @brief One row as the screen states it, to create, correct or reorder.
 */
struct row_body final {
    std::optional<std::string> code;
    std::optional<std::string> name;
    std::optional<std::string> description;
    std::optional<int> displayOrder;
    std::optional<std::uint32_t> version;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<row_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"code", &row_body::code},
                                              field{"name", &row_body::name},
                                              field{"description", &row_body::description},
                                              field{"displayOrder", &row_body::displayOrder},
                                              field{"version", &row_body::version}};
}

/**
 * @brief A write of one row: the row, and why.
 */
struct write_body final {
    std::optional<std::string> code;
    std::optional<std::string> name;
    std::optional<std::string> description;
    std::optional<int> displayOrder;
    std::optional<std::uint32_t> version;
    std::string reasonCode;
    std::optional<std::string> commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<write_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"code", &write_body::code},
                                              field{"name", &write_body::name},
                                              field{"description", &write_body::description},
                                              field{"displayOrder", &write_body::displayOrder},
                                              field{"version", &write_body::version},
                                              field{"reasonCode", &write_body::reasonCode},
                                              field{"commentary", &write_body::commentary}};
}

/**
 * @brief A new display order: the rows that moved, each with its version, and why.
 */
struct order_body final {
    std::vector<row_body> rows;
    std::string reasonCode;
    std::optional<std::string> commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<order_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"rows", &order_body::rows},
                                              field{"reasonCode", &order_body::reasonCode},
                                              field{"commentary", &order_body::commentary}};
}

/**
 * @brief A row's label: a badge from the catalogue, or none to take it away, and why.
 */
struct label_body final {
    std::optional<std::string> badgeCode;
    std::string reasonCode;
    std::optional<std::string> commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<label_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"badgeCode", &label_body::badgeCode},
                                              field{"reasonCode", &label_body::reasonCode},
                                              field{"commentary", &label_body::commentary}};
}

/**
 * @brief A removal of one row: why.
 */
struct removal_body final {
    std::string reasonCode;
    std::optional<std::string> commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<removal_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"reasonCode", &removal_body::reasonCode},
                                              field{"commentary", &removal_body::commentary}};
}

/**
 * @brief Whether an optional text the screen states fits: absent, or 2000 characters at most.
 */
inline bool text_fits(const std::optional<std::string>& value) {
    return !value || value->size() <= 2000;
}

/**
 * @brief Why a write is made: the reason trimmed, and the commentary or nothing.
 */
inline ores::utility::domain::change_intent
intent_of(const std::string& reason, const std::optional<std::string>& commentary) {
    const auto first = reason.find_first_not_of(" \t\r\n");
    ores::utility::domain::change_intent intent;
    intent.reason_code = first == std::string::npos ?
                             std::string{} :
                             reason.substr(first, reason.find_last_not_of(" \t\r\n") - first + 1);
    intent.commentary = commentary.value_or("");
    return intent;
}

/**
 * @brief The display order a row sorts by: its own, or zero for a list with none.
 */
template <typename Row>
int display_order_of(const Row& row) {
    if constexpr (requires { row.display_order; })
        return row.display_order;
    else
        return 0;
}

/**
 * @brief The rows in the order the screen lists them: by display order, then by code.
 */
template <typename Row>
void sort_rows(std::vector<Row>& rows) {
    std::sort(rows.begin(), rows.end(), [](const Row& a, const Row& b) {
        const auto oa = display_order_of(a);
        const auto ob = display_order_of(b);
        return oa != ob ? oa < ob : a.code < b.code;
    });
}

/**
 * @brief One row as the screen reads it, with the label its code is painted with.
 */
template <typename Row>
std::string row_json(const Row& row, const std::map<std::string, std::string>& labels) {
    namespace json = ores::utility::codec::json;
    std::string out = "{\"code\":" + json::write(row.code);
    if constexpr (requires { row.name; })
        out += ",\"name\":" + json::write(row.name);
    else
        out += ",\"name\":\"\"";
    out += ",\"description\":" + json::write(row.description);
    if constexpr (requires { row.display_order; })
        out += ",\"displayOrder\":" + std::to_string(row.display_order);
    else
        out += ",\"displayOrder\":null";
    out += ",\"version\":" + std::to_string(row.version);
    out += ",\"modifiedBy\":" + json::write(row.modified_by);
    out += ",\"recordedAt\":" + json::write(row.recorded_at);
    out += ",\"reasonCode\":" + json::write(row.change_reason_code);
    out += ",\"commentary\":" + json::write(row.change_commentary);
    const auto label = labels.find(row.code);
    out += ",\"labelCode\":" +
           (label == labels.end() ? std::string("null") : json::write(label->second));
    return out + "}";
}

/**
 * @brief The write a row becomes, with the members its list has.
 */
template <typename Write>
Write write_of(const std::string& code,
               const std::optional<std::string>& name,
               const std::optional<std::string>& description,
               std::optional<int> display_order) {
    Write write;
    write.code = code;
    if constexpr (requires { write.name; })
        write.name = name.value_or("");
    write.description = description.value_or("");
    if constexpr (requires { write.display_order; })
        write.display_order = display_order.value_or(0);
    return write;
}

}

#endif
