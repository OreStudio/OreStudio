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
#include "ores.http/routes/refdata/calendar_screen_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/domain/calendar_date_codec.hpp"
#include "ores.refdata.api/messaging/calendar_date_protocol.hpp"
#include "ores.refdata.api/messaging/calendar_materialisation_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <charconv>
#include <chrono>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <type_traits>

namespace ores::http::routes::refdata {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace forwarding = ores::http::forwarding;
namespace json = ores::utility::codec::json;
namespace messaging = ores::refdata::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

/// The first and last year a calendar is read or built for.
constexpr int calendar_screen_first_year = 1900;
constexpr int calendar_screen_last_year = 2200;

/// How many days one page of a calendar holds.
constexpr std::uint32_t calendar_screen_page = 1000;

/// The bodies the screen sends, beside the wire_fields each is read with.
namespace calendar_screen {

/// The build of a calendar's days: the last year to build.
struct rebuild_body final {
    int endYear{};
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<rebuild_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"endYear", &rebuild_body::endYear}};
}

}

bool is_calendar_year(int year) {
    return year >= calendar_screen_first_year && year <= calendar_screen_last_year;
}

/// The calendar the address names.
std::string calendar_code_of(const http_request& req) {
    return rr::code(req.get_path_param("code"),
                    "A calendar is named by its code, 1 to 100 characters.");
}

/// A failed operation, answered as the BFF answered it: a conflict with the service's words.
[[noreturn]] void operation_failed(const std::string& message) {
    rr::refuse(http_status::conflict, "invalid-request", message);
}

}

void calendar_screen_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"calendars"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/refdata/calendars/{code}/days")
            .summary("Read one calendar's built days in one year")
            .query_param("year", "integer", "", true, "The year, 1900 to 2200")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::list_by_calendar_code_calendar_dates_request>([&] {
                    const auto code = calendar_code_of(req);
                    const auto raw = req.get_query_param("year");
                    int requested_year = 0;
                    const auto [end, error] =
                        std::from_chars(raw.data(), raw.data() + raw.size(), requested_year);
                    if (raw.empty() || error != std::errc{} || end != raw.data() + raw.size() ||
                        !is_calendar_year(requested_year))
                        rr::invalid("The days are read one year at a time, between 1900 and 2200.");

                    using namespace std::chrono;
                    const year_month_day year_start{
                        std::chrono::year{requested_year}, January, day{1}};
                    const year_month_day year_end{
                        std::chrono::year{requested_year}, December, day{31}};
                    messaging::list_by_calendar_code_calendar_dates_request msg;
                    msg.calendar_code = code;
                    msg.scope = ores::utility::domain::scope::direct;
                    msg.limit = calendar_screen_page;

                    // The days come in date order, so the read stops at the
                    // first page that runs past the year.
                    std::string days = "[";
                    for (std::uint32_t page = 0;; ++page) {
                        if (page == calendar_screen_page)
                            throw std::runtime_error(
                                "The service sent more pages than a year of days takes.");
                        msg.offset = page * calendar_screen_page;
                        const auto reply = forwarding::forward(req, session, msg);
                        if (reply.result.outcome != ores::utility::domain::outcome::ok)
                            operation_failed(reply.result.message);
                        for (const auto& row : reply.calendar_dates) {
                            if (row.date < year_start || row.date > year_end)
                                continue;
                            days += days.size() == 1 ? "{" : ",{";
                            days += "\"date\":" + json::write(row.date);
                            days += std::string(",\"businessDay\":") +
                                    (row.is_business_day ? "true" : "false");
                            days += ",\"source\":" + json::write(row.source) + "}";
                        }
                        if (reply.calendar_dates.size() < calendar_screen_page ||
                            reply.calendar_dates.back().date > year_end)
                            break;
                    }
                    return http_response::json("{\"days\":" + days + "]}");
                });
            }));

    add(router->post("/api/refdata/calendars/{code}/rebuild")
            .summary("Build one calendar's days up to the end of a year")
            .description("A day already built keeps the value it was built with.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::regenerate_calendar_dates_request>([&] {
                    messaging::regenerate_calendar_dates_request msg;
                    msg.calendar_code = calendar_code_of(req);
                    const auto body = rr::read_body<calendar_screen::rebuild_body>(req.body);
                    if (!body || !is_calendar_year(body->endYear))
                        rr::invalid(
                            "A rebuild names the last year to build, between 1900 and 2200.");
                    msg.end_year = body->endYear;
                    const auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success)
                        operation_failed(reply.message);
                    return http_response::json(
                        "{\"written\":" + std::to_string(reply.rows_written) + "}");
                });
            }));
}

}
