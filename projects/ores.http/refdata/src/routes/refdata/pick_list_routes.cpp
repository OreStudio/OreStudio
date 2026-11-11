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
#include "ores.http/routes/refdata/pick_list_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/domain/book_purpose_type_codec.hpp"
#include "ores.refdata.api/domain/book_status_codec.hpp"
#include "ores.refdata.api/domain/business_centre_codec.hpp"
#include "ores.refdata.api/domain/business_day_convention_type_codec.hpp"
#include "ores.refdata.api/domain/business_unit_codec.hpp"
#include "ores.refdata.api/domain/calendar_codec.hpp"
#include "ores.refdata.api/domain/calendar_name_codec.hpp"
#include "ores.refdata.api/domain/contact_type_codec.hpp"
#include "ores.refdata.api/domain/currency_codec.hpp"
#include "ores.refdata.api/domain/day_count_fraction_type_codec.hpp"
#include "ores.refdata.api/domain/floating_index_type_codec.hpp"
#include "ores.refdata.api/domain/ledger_feed_type_codec.hpp"
#include "ores.refdata.api/domain/party_id_scheme_codec.hpp"
#include "ores.refdata.api/domain/party_status_codec.hpp"
#include "ores.refdata.api/domain/party_type_codec.hpp"
#include "ores.refdata.api/domain/payment_frequency_codec.hpp"
#include "ores.refdata.api/domain/purpose_type_codec.hpp"
#include "ores.refdata.api/domain/regulatory_book_type_codec.hpp"
#include "ores.refdata.api/domain/sub_periods_coupon_type_codec.hpp"
#include "ores.refdata.api/domain/tenor_anchor_codec.hpp"
#include "ores.refdata.api/domain/tenor_kind_codec.hpp"
#include "ores.refdata.api/domain/tenor_resolution_algorithm_codec.hpp"
#include "ores.refdata.api/domain/tenor_unit_codec.hpp"
#include "ores.refdata.api/messaging/book_purpose_type_protocol.hpp"
#include "ores.refdata.api/messaging/book_status_protocol.hpp"
#include "ores.refdata.api/messaging/business_centre_protocol.hpp"
#include "ores.refdata.api/messaging/business_day_convention_type_protocol.hpp"
#include "ores.refdata.api/messaging/business_unit_protocol.hpp"
#include "ores.refdata.api/messaging/calendar_name_protocol.hpp"
#include "ores.refdata.api/messaging/calendar_protocol.hpp"
#include "ores.refdata.api/messaging/contact_type_protocol.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/day_count_fraction_type_protocol.hpp"
#include "ores.refdata.api/messaging/floating_index_type_protocol.hpp"
#include "ores.refdata.api/messaging/ledger_feed_type_protocol.hpp"
#include "ores.refdata.api/messaging/party_id_scheme_protocol.hpp"
#include "ores.refdata.api/messaging/party_status_protocol.hpp"
#include "ores.refdata.api/messaging/party_type_protocol.hpp"
#include "ores.refdata.api/messaging/payment_frequency_protocol.hpp"
#include "ores.refdata.api/messaging/purpose_type_protocol.hpp"
#include "ores.refdata.api/messaging/regulatory_book_type_protocol.hpp"
#include "ores.refdata.api/messaging/sub_periods_coupon_type_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_anchor_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_kind_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_resolution_algorithm_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_unit_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <cstddef>
#include <cstdint>
#include <exception>
#include <memory>
#include <span>
#include <string>
#include <string_view>

namespace ores::http::routes::refdata {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace messaging = ores::refdata::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

/// How many rows one page asks for, as the BFF read them.
constexpr std::uint32_t pick_list_page = 1000;

/**
 * @brief Every row of one list, as the service sends it, read a page at a time.
 *
 * The read advances by the rows that arrived and ends on an empty page, or on a
 * short one once it holds the total.
 */
template <typename Request, typename Rows>
std::string all_rows_json(const http_request& req,
                          nats_client& session,
                          Rows Request::response_type::* rows,
                          Request msg) {
    std::string out = "[";
    std::uint64_t held = 0;
    for (std::uint32_t offset = 0;;) {
        msg.offset = offset;
        msg.limit = pick_list_page;
        msg.order.field = "";
        msg.order.descending = false;
        const auto reply = ores::http::forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        const auto& got = reply.*rows;
        for (const auto& row : got) {
            if (held++ != 0)
                out += ",";
            out += ores::utility::codec::json::write(row);
        }
        offset += static_cast<std::uint32_t>(got.size());
        if (got.empty() || (got.size() < pick_list_page && held >= reply.total))
            break;
    }
    return out + "]";
}

/// One control's list: the name the form keys it by, and its read.
struct pick_list final {
    std::string_view key;
    std::string (*read)(const http_request&, nats_client&);
};

/// The books form's controls, each with the list it offers.
const pick_list books_pick_lists[]{
    {"bookStatuses",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_book_statuses_response::statuses,
                              messaging::list_book_statuses_request{});
     }},
    {"regulatoryBookTypes",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_regulatory_book_types_response::types,
                              messaging::list_regulatory_book_types_request{});
     }},
    {"bookPurposeTypes",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_book_purpose_types_response::types,
                              messaging::list_book_purpose_types_request{});
     }},
    {"ledgerFeedTypes",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_ledger_feed_types_response::types,
                              messaging::list_ledger_feed_types_request{});
     }},
    {"purposeTypes",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_purpose_types_response::types,
                              messaging::list_purpose_types_request{});
     }},
    {"currencies",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_currencies_response::currencies,
                              messaging::list_currencies_request{});
     }},
    {"businessCentres",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_business_centres_response::centres,
                              messaging::list_business_centres_request{});
     }},
    {"businessUnits",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_business_units_response::business_units,
                              messaging::list_business_units_request{});
     }},
};

/// The conventions form's controls, each with the list it offers.
const pick_list conventions_pick_lists[]{
    {"calendars",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_calendar_names_response::calendar_names,
                              messaging::list_calendar_names_request{});
     }},
    {"businessDayConventions",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_business_day_convention_types_response::types,
                              messaging::list_business_day_convention_types_request{});
     }},
    {"dayCountFractions",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_day_count_fraction_types_response::types,
                              messaging::list_day_count_fraction_types_request{});
     }},
    {"floatingIndices",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_floating_index_types_response::types,
                              messaging::list_floating_index_types_request{});
     }},
    {"paymentFrequencies",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_payment_frequencies_response::payment_frequencies,
                              messaging::list_payment_frequencies_request{});
     }},
    {"subPeriodsCouponTypes",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_sub_periods_coupon_types_response::types,
                              messaging::list_sub_periods_coupon_types_request{});
     }},
    {"currencies",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_currencies_response::currencies,
                              messaging::list_currencies_request{});
     }},
};

/// The counterparties form's controls, each with the list it offers.
const pick_list counterparties_pick_lists[]{
    {"partyTypes",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_party_types_response::types,
                              messaging::list_party_types_request{});
     }},
    {"partyStatuses",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_party_statuses_response::statuses,
                              messaging::list_party_statuses_request{});
     }},
    {"identifierSchemes",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_party_id_schemes_response::schemes,
                              messaging::list_party_id_schemes_request{});
     }},
    {"contactTypes",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_contact_types_response::types,
                              messaging::list_contact_types_request{});
     }},
    {"businessCentres",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_business_centres_response::centres,
                              messaging::list_business_centres_request{});
     }},
    {"currencies",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_currencies_response::currencies,
                              messaging::list_currencies_request{});
     }},
};

/// The tenors form's controls, each with the list it offers.
const pick_list tenors_pick_lists[]{
    {"kinds",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_tenor_kinds_response::kinds,
                              messaging::list_tenor_kinds_request{});
     }},
    {"units",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_tenor_units_response::units,
                              messaging::list_tenor_units_request{});
     }},
    {"anchors",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_tenor_anchors_response::anchors,
                              messaging::list_tenor_anchors_request{});
     }},
    {"algorithms",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_tenor_resolution_algorithms_response::algorithms,
                              messaging::list_tenor_resolution_algorithms_request{});
     }},
    {"rollRules",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_business_day_convention_types_response::types,
                              messaging::list_business_day_convention_types_request{});
     }},
    {"calendars",
     [](const http_request& req, nats_client& session) {
         return all_rows_json(req,
                              session,
                              &messaging::list_calendars_response::calendars,
                              messaging::list_calendars_request{});
     }},
};

/// Every list a form offers, keyed by the control's name.
boost::asio::awaitable<http_response>
answer(const http_request& req, nats_client& session, std::span<const pick_list> lists) {
    try {
        std::string out = "{";
        for (std::size_t i = 0; i < lists.size(); ++i) {
            if (i != 0)
                out += ",";
            out += ores::utility::codec::json::write(std::string(lists[i].key)) + ":" +
                   lists[i].read(req, session);
        }
        co_return http_response::json(out + "}");
    } catch (const rr::refusal& refusal) {
        co_return rr::answer(refusal);
    } catch (const std::exception& e) {
        co_return ores::http::forwarding::unanswered("a pick list", e);
    }
}

/// One form: the address it reads its lists at, and the lists.
struct pick_list_form final {
    std::string_view path;
    std::string_view summary;
    std::span<const pick_list> lists;
};

const pick_list_form forms[]{
    {"/api/books/pick-lists", "Read the books form's option lists", books_pick_lists},
    {"/api/conventions/pick-lists",
     "Read the conventions form's option lists",
     conventions_pick_lists},
    {"/api/counterparties/pick-lists",
     "Read the counterparties form's option lists",
     counterparties_pick_lists},
    {"/api/tenors/pick-lists", "Read the tenors form's option lists", tenors_pick_lists},
};

}

void pick_list_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    for (const auto& form : forms) {
        auto route = router->get(std::string(form.path))
                         .summary(std::string(form.summary))
                         .description("Every row of each list the form offers, by the control's name.")
                         .tags({"refdata"})
                         .auth_required()
                         .handler([&session, lists = form.lists](const http_request& req) {
                             return answer(req, session, lists);
                         });
        const auto built = route.build();
        router->add_route(built);
        registry->register_route(built);
    }
}

}
