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
#include "ores.http/routes/reporting/choice_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/messaging/regulatory_book_type_protocol.hpp"
#include "ores.reporting.api/messaging/concurrency_policy_protocol.hpp"
#include "ores.reporting.api/messaging/report_type_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <string>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace refdata_messaging = ores::refdata::messaging;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The unit's own helpers, named apart from the other units that build into
/// one translation unit with it.
namespace choices {

/// Every code a list of rows states, without the rows that state none.
template <typename Row>
std::vector<std::string> codes_of(const std::vector<Row>& rows) {
    std::vector<std::string> codes;
    for (const auto& row : rows)
        if (!row.code.empty())
            codes.push_back(row.code);
    return codes;
}

/// Every row of a list, or none when the caller may not read it.
///
/// Only a refusal means the account may not read the list. An outage is not the
/// same thing and must not read as "no codes available", so it travels.
template <typename Request, typename Rows>
auto readable(const http_request& req, nats_client& session, Request msg, Rows member) {
    try {
        return forwarding::forward_all(req, session, std::move(msg), member);
    } catch (const rr::refusal& r) {
        if (r.status != http_status::forbidden)
            throw;
    }
    return std::remove_cvref_t<decltype(std::declval<typename Request::response_type>().*member)>{};
}

}

}

std::string choice_routes::choices_json(const std::vector<std::string>& report_types,
                                        const std::vector<std::string>& concurrency_policies,
                                        const std::vector<std::string>& regulatory_book_types) {
    return "{\"reportTypes\":" + json::write(report_types) +
           ",\"concurrencyPolicies\":" + json::write(concurrency_policies) +
           ",\"regulatoryBookTypes\":" + json::write(regulatory_book_types) + "}";
}

void choice_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace choices;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/choices")
            .summary("The codes a report's edit form offers")
            .description("The report types, the concurrency policies a report may be published "
                         "under, and the regulatory book types it may scope by. The book types "
                         "are reference data, so an account that may not read them writes no "
                         "report at all and the answer states none; any other failure travels.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_report_types_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto types = forwarding::forward_all(
                        req,
                        session,
                        reporting_messaging::list_report_types_request{},
                        &reporting_messaging::list_report_types_response::types);
                    const auto policies = forwarding::forward_all(
                        req,
                        session,
                        reporting_messaging::list_concurrency_policies_request{},
                        &reporting_messaging::list_concurrency_policies_response::policies);
                    const auto book_types =
                        readable(req,
                                 session,
                                 refdata_messaging::list_regulatory_book_types_request{},
                                 &refdata_messaging::list_regulatory_book_types_response::types);
                    return http_response::json(
                        choices_json(codes_of(types), codes_of(policies), codes_of(book_types)));
                });
            }));
}

}
