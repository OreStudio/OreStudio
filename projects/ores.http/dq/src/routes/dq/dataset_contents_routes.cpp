/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
#include "ores.http/routes/dq/dataset_contents_routes.hpp"
#include "ores.dq.api/messaging/dataset_contents_protocol.hpp"
#include "ores.http.nats/forwarding.hpp"
#include <charconv>
#include <stdexcept>

namespace ores::http::routes::dq {

using namespace ores::logging;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace messaging = ores::dq::messaging;

namespace {

/**
 * @brief Reads one whole integer a caller stated, or refuses it.
 *
 * A page that silently fell back to a default would answer rows the caller did
 * not ask for, so a value that does not parse is an error.
 */
int read_int(const std::string& raw, const char* name) {
    int value = 0;
    const auto* first = raw.data();
    const auto* last = raw.data() + raw.size();
    const auto [ptr, ec] = std::from_chars(first, last, value);
    if (ec != std::errc{} || ptr != last)
        throw std::invalid_argument(std::string("Invalid ") + name + ": " + raw);
    return value;
}

}

void dataset_contents_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    ORES_LOG_SEV(lg(), debug) << "Registering the dataset contents route";

    auto contents_route =
        router->get("/api/v1/dq/dataset_contents")
            .summary("Read what a dataset has staged")
            .description("Forwards to the dq.v1.ops.dataset_contents operation: the tables the "
                         "dataset's artefact type names, each with its own columns and the rows "
                         "it holds; and, when a table is named, one page of that table's rows "
                         "with the columns they carry.")
            .tags({"dq"})
            .operation_catalogue("dq", "dataset_contents", "dataset_contents_read")
            .auth_required()
            .query_param("dataset_code", "string", "", true, "The dataset whose contents to read")
            .query_param("table_name",
                         "string",
                         "",
                         false,
                         "The table whose rows to read, or empty for the list of tables alone")
            .query_param("limit", "integer", "15", false, "The most rows to answer")
            .query_param("offset", "integer", "0", false, "Rows to skip before the ones answered")
            .handler([&session](const http_request& req) { return handle_contents(req, session); });
    const auto built = contents_route.build();
    router->add_route(built);
    registry->register_route(built);
}

boost::asio::awaitable<http_response>
dataset_contents_routes::handle_contents(const http_request& req, nats_client& session) {
    try {
        messaging::dataset_contents_request msg;
        msg.dataset_code = req.get_query_param("dataset_code");
        msg.table_name = req.get_query_param("table_name");
        if (const auto raw = req.get_query_param("limit"); !raw.empty())
            msg.limit = read_int(raw, "limit");
        if (const auto raw = req.get_query_param("offset"); !raw.empty())
            msg.offset = read_int(raw, "offset");
        co_return ores::http::forwarding::respond(req, session, msg);
    } catch (const std::exception& e) {
        co_return http_response::api_error(
            ores::http::domain::http_status::bad_request, "invalid-request", e.what());
    }
}

} // namespace ores::http::routes::dq
