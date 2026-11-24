/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/domain/catalogue_rules.hpp"
#include <algorithm>
#include <sstream>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::http::net {

using namespace ores::logging;

namespace {

/**
 * @brief A pattern's standing against another that matches the same path: one
 * entry per segment, a literal ranked above a parameter. Compared in order, so
 * the first segment that differs decides, and a longer pattern beats its prefix.
 */
std::vector<int> specificity_of(const std::string& pattern) {
    std::vector<int> rank;
    std::size_t start = 0;
    while (start <= pattern.size()) {
        const auto end = std::min(pattern.find('/', start), pattern.size());
        const auto segment = std::string_view(pattern).substr(start, end - start);
        if (!segment.empty())
            rank.push_back(segment.front() == '{' ? 1 : 2);
        start = end + 1;
    }
    return rank;
}

/**
 * @brief Converts a path pattern like "/users/{id}" to a regex and extracts param names.
 */
std::pair<std::regex, std::vector<std::string>> compile_pattern(const std::string& pattern) {

    std::vector<std::string> param_names;
    std::ostringstream regex_str;
    regex_str << "^";

    std::size_t pos = 0;
    while (pos < pattern.size()) {
        if (pattern[pos] == '{') {
            auto end = pattern.find('}', pos);
            if (end != std::string::npos) {
                std::string param_name = pattern.substr(pos + 1, end - pos - 1);
                param_names.push_back(param_name);
                // If this is the last segment in the pattern, use a greedy
                // match so keys like "compute/packages/uuid.tar.gz" work.
                if (end + 1 >= pattern.size())
                    regex_str << "(.+)";
                else
                    regex_str << "([^/]+)";
                pos = end + 1;
                continue;
            }
        }
        // Escape regex special characters
        char c = pattern[pos];
        if (c == '.' || c == '+' || c == '*' || c == '?' || c == '(' || c == ')' || c == '[' ||
            c == ']' || c == '{' || c == '}' || c == '$' || c == '^' || c == '|' || c == '\\') {
            regex_str << '\\';
        }
        regex_str << c;
        ++pos;
    }

    regex_str << "$";
    return {std::regex(regex_str.str()), param_names};
}

/**
 * @brief The failure a route with no stated authentication position names.
 */
std::logic_error undeclared_auth(const domain::http_method method, const std::string& pattern) {
    return std::logic_error("Route " + std::string(domain::method_name(method)) + " " + pattern +
                            " declares no authentication position; call auth_required() or "
                            "auth_optional() before build().");
}

}

// route_builder implementation

route_builder& route_builder::json_body() {
    domain::request_body_schema schema;
    schema.content_type = "application/json";
    schema.required = true;
    schema.json_schema = R"({"type":"object"})";
    body_schema_ = schema;
    return *this;
}

route_builder& route_builder::transport() {
    catalogue_.transport = true;
    return *this;
}

route_builder& route_builder::no_body() {
    catalogue_.takes_no_body = true;
    return *this;
}

route_builder& route_builder::place(domain::route_catalogue placed) {
    // The place replaces where the route belongs and what it needs, and keeps
    // the documentation, whichever was stated first.
    catalogue_.tool = std::move(placed.tool);
    catalogue_.area = std::move(placed.area);
    catalogue_.entity = std::move(placed.entity);
    catalogue_.writes = placed.writes;
    catalogue_.permission = std::move(placed.permission);
    return *this;
}

route_builder& route_builder::entity_catalogue(std::string_view component,
                                               std::string_view entity,
                                               std::string_view identifier) {
    return place(domain::entity_route_catalogue(method_, pattern_, component, entity, identifier));
}

route_builder& route_builder::operation_catalogue(std::string_view component,
                                                  std::string_view entity,
                                                  std::string_view name) {
    return place(domain::operation_route_catalogue(component, entity, name));
}

route_builder&
route_builder::catalogue(std::string tool, std::string area, std::string entity, bool writes) {
    catalogue_.tool = std::move(tool);
    catalogue_.area = std::move(area);
    catalogue_.entity = std::move(entity);
    catalogue_.writes = writes;
    return *this;
}

route_builder::route_builder(domain::http_method method, const std::string& pattern)
    : method_(method)
    , pattern_(pattern) {}

route_builder& route_builder::handler(domain::request_handler h) {
    handler_ = std::move(h);
    return *this;
}

route_builder& route_builder::auth_required() {
    requires_auth_ = true;
    auth_declared_ = true;
    return *this;
}

route_builder& route_builder::auth_optional() {
    requires_auth_ = false;
    auth_declared_ = true;
    return *this;
}

route_builder& route_builder::roles(std::vector<std::string> r) {
    required_roles_ = std::move(r);
    // Naming roles is a statement that the route is authenticated.
    requires_auth_ = true;
    auth_declared_ = true;
    return *this;
}

route_builder& route_builder::max_body(std::uint64_t bytes) {
    max_body_size_ = bytes;
    return *this;
}

route_builder& route_builder::rate_limit(std::string_view policy) {
    rate_policy_ = std::string(policy);
    return *this;
}

route_builder& route_builder::sink(
    std::function<std::unique_ptr<domain::body_sink>(const domain::http_request&)> factory) {
    sink_ = std::move(factory);
    return *this;
}

route_builder& route_builder::summary(std::string s) {
    catalogue_.summary = std::move(s);
    return *this;
}

route_builder& route_builder::description(std::string d) {
    catalogue_.description = std::move(d);
    return *this;
}

route_builder& route_builder::tags(std::vector<std::string> t) {
    catalogue_.tags = std::move(t);
    return *this;
}

route_builder& route_builder::query_param(const std::string& name,
                                          const std::string& type,
                                          const std::string& format,
                                          bool required,
                                          const std::string& desc,
                                          const std::optional<std::string>& default_value) {

    domain::query_param param;
    param.name = name;
    param.type = type;
    param.format = format;
    param.required = required;
    param.description = desc;
    param.default_value = default_value;
    query_params_.push_back(param);
    return *this;
}

domain::route route_builder::build() const {
    if (!auth_declared_)
        throw undeclared_auth(method_, pattern_);

    auto [regex, param_names] = compile_pattern(pattern_);

    domain::route r;
    r.method = method_;
    r.pattern = pattern_;
    r.regex = std::move(regex);
    r.param_names = std::move(param_names);
    r.handler = handler_;
    r.requires_auth = requires_auth_;
    r.auth_declared = true;
    r.required_roles = required_roles_;
    r.catalogue = catalogue_;
    r.query_params = query_params_;
    r.sink = sink_;
    r.max_body_size = max_body_size_;
    r.rate_policy = rate_policy_;
    r.body_schema = body_schema_;
    r.success_response_schema = success_response_schema_;
    return r;
}

// router implementation

router::router() {
    ORES_LOG_SEV(lg(), debug) << "Router created";
}

route_builder router::get(const std::string& pattern) {
    return route_builder(domain::http_method::get, pattern);
}

route_builder router::post(const std::string& pattern) {
    return route_builder(domain::http_method::post, pattern);
}

route_builder router::put(const std::string& pattern) {
    return route_builder(domain::http_method::put, pattern);
}

route_builder router::patch(const std::string& pattern) {
    return route_builder(domain::http_method::patch, pattern);
}

route_builder router::delete_(const std::string& pattern) {
    return route_builder(domain::http_method::delete_, pattern);
}

route_builder router::head(const std::string& pattern) {
    return route_builder(domain::http_method::head, pattern);
}

void router::add_route(const domain::route& route) {
    if (!route.auth_declared)
        throw undeclared_auth(route.method, route.pattern);

    ORES_LOG_SEV(lg(), info) << "Registered route: " << static_cast<int>(route.method) << " "
                             << route.pattern;
    routes_.push_back(route);
    ranks_.push_back(specificity_of(route.pattern));
}

std::optional<domain::route>
router::match(domain::http_method method,
              const std::string& path,
              std::unordered_map<std::string, std::string>& path_params) const {

    ORES_LOG_SEV(lg(), trace) << "Matching path: " << path
                              << " method: " << static_cast<int>(method);

    /*
     * A path two patterns match goes to the more specific one, as the BFF's
     * framework sent it: a literal segment beats a parameter at the first place
     * they differ, so /parties/children never reaches /parties/{id}. Order of
     * registration decides only between patterns of equal standing.
     *
     * A HEAD is answered by the GET of the same pattern when no HEAD route
     * serves it, which is how every read becomes probeable without a second
     * declaration per route. The pattern's standing still decides first, so a
     * literal path beats a parameter whether the answer comes from a HEAD or
     * from its GET; a declared HEAD route wins only where the patterns stand
     * equally.
     */
    const bool answering_head = method == domain::http_method::head;
    const domain::route* best = nullptr;
    const std::vector<int>* best_rank = nullptr;
    bool best_names_the_method = false;
    std::unordered_map<std::string, std::string> best_params;
    std::unordered_map<std::string, std::string> params;
    for (std::size_t i = 0; i < routes_.size(); ++i) {
        const auto& route = routes_[i];
        const bool names_the_method = route.method == method;
        if (!names_the_method && !(answering_head && route.method == domain::http_method::get))
            continue;
        params.clear();
        if (!route.match(path, params))
            continue;
        // The standing of the pattern decides; the method only breaks a tie.
        const bool beats = best == nullptr || ranks_[i] > *best_rank ||
                           (ranks_[i] == *best_rank && names_the_method && !best_names_the_method);
        if (beats) {
            best = &route;
            best_rank = &ranks_[i];
            best_names_the_method = names_the_method;
            std::swap(best_params, params);
        }
    }

    if (best == nullptr) {
        ORES_LOG_SEV(lg(), trace) << "No matching route found for: " << path;
        return std::nullopt;
    }
    ORES_LOG_SEV(lg(), trace) << "Matched route: " << best->pattern;
    for (auto& [name, value] : best_params)
        path_params[name] = std::move(value);
    return *best;
}

std::vector<std::string> router::uncatalogued() const {
    std::vector<std::string> out;
    std::unordered_map<std::string, std::string> first_with_tool;
    for (const auto& route : routes_) {
        const auto name = std::string(domain::method_name(route.method)) + " " + route.pattern;
        if (!route.catalogue.declared()) {
            out.push_back(name);
            continue;
        }
        if (!domain::is_published_area(route.catalogue.area))
            out.push_back(name + " names the area " + route.catalogue.area +
                          ", which is not a published one");
        // A route that changes something by its method writes unless it is
        // one of the two kinds that only read, so no write is declared out of
        // the hold that keeps an agent's writes for its person.
        const bool changes = route.method != domain::http_method::get &&
                             route.method != domain::http_method::head &&
                             route.method != domain::http_method::options;
        const bool carries_body = route.method == domain::http_method::post ||
                                  route.method == domain::http_method::put ||
                                  route.method == domain::http_method::patch ||
                                  route.method == domain::http_method::delete_;
        if (carries_body && !route.body_schema && !route.sink && !route.catalogue.takes_no_body)
            out.push_back(name + " declares neither its typed body nor that it takes none");
        if (route.catalogue.takes_no_body && (route.body_schema || route.sink))
            out.push_back(name + " declares that it takes no body, and also a body");
        if (changes && !route.catalogue.writes && !route.catalogue.transport &&
            !domain::may_declare_read(route.catalogue))
            out.push_back(name + " declares that it does not write, and its tool " +
                          route.catalogue.tool + " is neither a bulk get nor a read");
        // A tool names one operation, so two routes may not share one.
        const auto [it, fresh] = first_with_tool.emplace(route.catalogue.tool, name);
        if (!fresh)
            out.push_back(name + " shares the tool " + route.catalogue.tool + " with " +
                          it->second);
    }
    return out;
}

}
