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
#ifndef ORES_HTTP_NET_ROUTER_HPP
#define ORES_HTTP_NET_ROUTER_HPP

#include "ores.http.api/domain/route.hpp"
#include "ores.http.api/export.hpp"
#include "ores.logging/logger.hpp"
#include "ores.utility/codec/json_schema.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::net {

/**
 * @brief Writes an example in the codec's form for its type: through its wire
 * traits where it has them, else through the fields it states, which is how a
 * route's own body type reaches the wire.
 */
template <typename T>
std::string example_json_of(const T& value) {
    if constexpr (requires { ores::utility::codec::wire_traits<T>::write_json(value); }) {
        return ores::utility::codec::wire_traits<T>::write_json(value);
    } else {
        static_assert(ores::utility::codec::states_wire_form<T>,
                      "A route's body or response type needs wire traits or wire_fields.");
        return ores::utility::codec::object::write_json(value);
    }
}

/**
 * @brief Route builder for fluent route registration.
 */
class ORES_HTTP_API_EXPORT route_builder final {
public:
    route_builder(domain::http_method method, const std::string& pattern);

    /**
     * @brief Sets the handler for this route.
     */
    route_builder& handler(domain::request_handler h);

    /**
     * @brief Marks this route as requiring a session.
     */
    route_builder& auth_required();

    /**
     * @brief Marks this route as deliberately reachable without a session.
     *
     * A route must state exactly one of auth_required() or auth_optional().
     * Stating neither is a mistake, not a default, so build() refuses rather
     * than letting the route read as public.
     */
    route_builder& auth_optional();

    /**
     * @brief Sets required roles for this route.
     *
     * Roles imply a session, so this states auth_required() as well.
     */
    route_builder& roles(std::vector<std::string> r);

    /**
     * @brief Sets the largest body this route will read.
     *
     * Zero, the default, means the server's default applies.
     */
    route_builder& max_body(std::uint64_t bytes);

    /**
     * @brief Runs this route under a named rate policy rather than the default
     * for its authentication position. The server refuses to start when no
     * policy has the name.
     */
    route_builder& rate_limit(std::string_view policy);

    /**
     * @brief Makes the body a stream, and says where it goes.
     *
     * A route that states no sink reads its body into the request. One that
     * states a factory gets a sink per request, filled in chunks by the session.
     */
    route_builder&
    sink(std::function<std::unique_ptr<domain::body_sink>(const domain::http_request&)> factory);

    /**
     * @brief Sets OpenAPI summary.
     */
    route_builder& summary(std::string s);

    /**
     * @brief Sets OpenAPI description.
     */
    route_builder& description(std::string d);

    /**
     * @brief Sets OpenAPI tags.
     */
    route_builder& tags(std::vector<std::string> t);

    /**
     * @brief The route's place in the operation catalogue: its stable tool
     * name, its published area, its entity and whether it writes.
     */
    route_builder& catalogue(std::string tool, std::string area, std::string entity, bool writes);

    /**
     * @brief States that a route whose method could carry a body takes none.
     */
    route_builder& no_body();

    /**
     * @brief States that a route takes a JSON object whose shape no one type
     * fixes, such as a document whose members depend on its family.
     */
    route_builder& json_body();

    /**
     * @brief States that the route is a transport that carries calls to other
     * routes, so it does not write whatever its method.
     */
    route_builder& transport();

    /**
     * @brief Takes a place in the catalogue, keeping the route's documentation.
     */
    route_builder& place(domain::route_catalogue placed);

    /**
     * @brief A generated entity route's place, from its model.
     */
    route_builder& entity_catalogue(std::string_view component,
                                    std::string_view entity,
                                    std::string_view identifier);

    /**
     * @brief A generated operation route's place, from its model.
     */
    route_builder&
    operation_catalogue(std::string_view component, std::string_view entity, std::string_view name);

    /**
     * @brief Adds a query parameter for OpenAPI documentation.
     */
    route_builder& query_param(const std::string& name,
                               const std::string& type = "string",
                               const std::string& format = "",
                               bool required = false,
                               const std::string& desc = "",
                               const std::optional<std::string>& default_value = std::nullopt);

    /**
     * @brief Sets the request body schema from a C++ type using reflection.
     *
     * The schema comes from the type's wire shape, so it states the bytes the
     * codec writes rather than a second description of them.
     * Optionally accepts a generator function to create example data.
     *
     * @tparam T The request body type (must have a wire codec)
     * @param example_generator Optional function that returns an example T
     * @param required Whether the body is required (default: true)
     * @param content_type Content type (default: application/json)
     */
    template <typename T>
    route_builder& body(std::function<T()> example_generator = nullptr,
                        bool required = true,
                        const std::string& content_type = "application/json") {

        domain::request_body_schema schema;
        schema.content_type = content_type;
        schema.required = required;
        schema.json_schema = ores::utility::codec::json_schema<T>();

        if (example_generator) {
            schema.example_json = example_json_of<T>(example_generator());
        }

        body_schema_ = schema;
        return *this;
    }

    /**
     * @brief Sets the response schema from a C++ type using reflection.
     *
     * The schema comes from the type's wire shape, so it states the bytes the
     * codec writes rather than a second description of them.
     * Optionally accepts a generator function to create example data.
     *
     * @tparam T The response body type (must have a wire codec)
     * @param example_generator Optional function that returns an example T
     * @param description Description of the response (default: "Successful response")
     * @param content_type Content type (default: application/json)
     */
    template <typename T>
    route_builder& response(std::function<T()> example_generator = nullptr,
                            const std::string& desc = "Successful response",
                            const std::string& content_type = "application/json") {

        domain::response_schema schema;
        schema.status_code = "200";
        schema.description = desc;
        schema.content_type = content_type;
        schema.json_schema = ores::utility::codec::json_schema<T>();

        if (example_generator) {
            schema.example_json = example_json_of<T>(example_generator());
        }

        success_response_schema_ = schema;
        return *this;
    }

    /**
     * @brief Builds the route.
     *
     * @throws std::logic_error when the route states no authentication
     * position, naming the method and the pattern it would have answered.
     */
    domain::route build() const;

private:
    domain::http_method method_;
    std::string pattern_;
    domain::request_handler handler_;
    bool requires_auth_ = false;
    bool auth_declared_ = false;
    std::vector<std::string> required_roles_;
    domain::route_catalogue catalogue_;
    std::vector<domain::query_param> query_params_;
    std::uint64_t max_body_size_ = 0;
    std::string rate_policy_;
    std::function<std::unique_ptr<domain::body_sink>(const domain::http_request&)> sink_;
    std::optional<domain::request_body_schema> body_schema_;
    std::optional<domain::response_schema> success_response_schema_;
};

/**
 * @brief HTTP request router that matches requests to handlers.
 */
class ORES_HTTP_API_EXPORT router final {
public:
    router();

    /**
     * @brief Registers a GET route.
     */
    route_builder get(const std::string& pattern);

    /**
     * @brief Registers a POST route.
     */
    route_builder post(const std::string& pattern);

    /**
     * @brief Registers a PUT route.
     */
    route_builder put(const std::string& pattern);

    /**
     * @brief Registers a PATCH route.
     */
    route_builder patch(const std::string& pattern);

    /**
     * @brief Registers a DELETE route.
     */
    route_builder delete_(const std::string& pattern);

    /**
     * @brief Registers a HEAD route.
     *
     * A HEAD route answers with the status and the headers a GET would carry,
     * and no body, so a caller can learn whether a resource exists and how
     * large it is without transferring it.
     *
     * A route need not be declared: every GET is answered over HEAD as well,
     * as stated at @ref match.
     */
    route_builder head(const std::string& pattern);

    /**
     * @brief Adds a built route to the router.
     *
     * @throws std::logic_error when the route states no authentication
     * position, naming the method and the pattern.
     */
    void add_route(const domain::route& route);

    /**
     * @brief Attempts to match a request and returns the matching route.
     *
     * When several patterns match, a literal segment beats a parameter at the
     * first segment where they differ, whatever order they were added in.
     *
     * A HEAD request is answered by the GET route of the same pattern when no
     * HEAD route serves it, so a probe reads the truth without every read
     * stating a second route, and a new route cannot forget to. The answer it
     * produces keeps the GET's status, headers and length, and the session
     * sends no body for a HEAD.
     */
    std::optional<domain::route>
    match(domain::http_method method,
          const std::string& path,
          std::unordered_map<std::string, std::string>& path_params) const;

    /**
     * @brief Every registered route the catalogue cannot accept, with the
     * reason. The server refuses to start with one.
     *
     * The router refuses a route for one of these reasons:
     *
     * - It declares no place in the catalogue.
     * - Its area is not a published area.
     * - It shares its tool with another route.
     * - Its method changes something, it declares that it does not write, and
     *   it is not a bulk get, an operation named for reading or a transport.
     *   This keeps every write inside the six-eyes hold.
     * - Its method can carry a body (POST, PUT, PATCH or DELETE), and it
     *   declares no typed body, no stream and no empty body. This makes every
     *   tool state its input.
     * - It declares an empty body together with a typed body or a stream.
     */
    [[nodiscard]] std::vector<std::string> uncatalogued() const;

    /**
     * @brief Returns all registered routes (for OpenAPI generation).
     */
    const std::vector<domain::route>& routes() const {
        return routes_;
    }

private:
    inline static std::string_view logger_name = "ores.http.net.router";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    std::vector<domain::route> routes_;

    /// Each route's specificity, at the same index as the route, worked out once when added.
    std::vector<std::vector<int>> ranks_;
};

}

#endif
