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
#ifndef ORES_HTTP_API_MCP_OPERATION_CATALOGUE_HPP
#define ORES_HTTP_API_MCP_OPERATION_CATALOGUE_HPP

#include "ores.http.api/domain/route.hpp"
#include "ores.http.api/export.hpp"
#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::http::mcp {

/**
 * @brief One operation an agent may find and call: a route, as the catalogue
 * describes it.
 */
struct operation final {
    std::string tool;
    std::string area;
    std::string entity;
    std::string summary;
    std::string description;
    bool writes = false;
    /// The permission its service checks, when the catalogue knows it.
    std::string permission;
    domain::http_method method = domain::http_method::get;
    std::string pattern;
    std::vector<std::string> path_params;
    std::vector<domain::query_param> query_params;
    std::optional<domain::request_body_schema> body;
    std::vector<std::string> required_roles;
    domain::request_handler handler;
};

/**
 * @brief The operations ores.http serves, as one agent walks them: by area,
 * then entity, then operation, or by words.
 *
 * It is a snapshot of the routes when it is made, without the transports and
 * the routes that stream a body, which a tool cannot carry.
 */
class ORES_HTTP_API_EXPORT operation_catalogue final {
public:
    explicit operation_catalogue(const std::vector<domain::route>& routes);

    [[nodiscard]] const std::vector<operation>& operations() const {
        return operations_;
    }

    [[nodiscard]] const operation* find(std::string_view tool) const;

    /**
     * @brief The operations that best match some words, best first, ranked by
     * BM25 over each operation's tool, area, entity, summary and description.
     * Only operations the filter admits are ranked.
     */
    [[nodiscard]] std::vector<const operation*>
    search(std::string_view words,
           std::size_t limit,
           const std::function<bool(const operation&)>& admits) const;

private:
    std::vector<operation> operations_;
    std::unordered_map<std::string, std::size_t> by_tool_;
    std::vector<std::unordered_map<std::string, std::size_t>> term_counts_;
    std::vector<std::size_t> lengths_;
    std::unordered_map<std::string, std::size_t> document_frequency_;
    double average_length_ = 0;
};

/**
 * @brief The words of a text as the search compares them: lower case, split
 * at every character that is not a letter or a digit, with a plural folded.
 */
ORES_HTTP_API_EXPORT std::vector<std::string> search_terms(std::string_view text);

/**
 * @brief An operation's arguments as a JSON Schema: its path parameters, its
 * query parameters and its body, each by name.
 */
ORES_HTTP_API_EXPORT std::string input_schema_of(const operation& op);

/**
 * @brief Arguments a schema accepts, with a placeholder in every value, so a
 * model sees the shape it must send.
 */
ORES_HTTP_API_EXPORT std::string example_of(const operation& op);

}

#endif
