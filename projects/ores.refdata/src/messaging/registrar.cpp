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
#include "ores.refdata/messaging/registrar.hpp"

#include <optional>
#include <span>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata/messaging/country_protocol.hpp"
#include "ores.refdata/messaging/currency_protocol.hpp"
#include "ores.refdata/messaging/party_protocol.hpp"
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.refdata/messaging/book_protocol.hpp"
#include "ores.refdata/messaging/portfolio_protocol.hpp"
#include "ores.refdata/messaging/business_unit_protocol.hpp"
#include "ores.refdata/messaging/contact_type_protocol.hpp"
#include "ores.refdata/messaging/monetary_nature_protocol.hpp"
#include "ores.refdata/messaging/rounding_type_protocol.hpp"
#include "ores.refdata/messaging/business_centre_protocol.hpp"
#include "ores.refdata/service/country_service.hpp"
#include "ores.refdata/service/currency_service.hpp"
#include "ores.refdata/service/party_service.hpp"
#include "ores.refdata/service/counterparty_service.hpp"
#include "ores.refdata/service/book_service.hpp"
#include "ores.refdata/service/portfolio_service.hpp"
#include "ores.refdata/service/business_unit_service.hpp"
#include "ores.refdata/service/contact_type_service.hpp"
#include "ores.refdata/service/monetary_nature_service.hpp"
#include "ores.refdata/service/rounding_type_service.hpp"
#include "ores.refdata/service/business_centre_service.hpp"
#include "ores.service/service/request_context.hpp"

namespace ores::refdata::messaging {

namespace {

template<typename Resp>
void reply(ores::nats::service::client& nats,
           const ores::nats::message& msg,
           const Resp& resp) {
    if (msg.reply_subject.empty())
        return;
    const auto json = rfl::json::write(resp);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(msg.reply_subject, std::span<const std::byte>(p, json.size()));
}

template<typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto r = rfl::json::read<Req>(sv);
    if (!r)
        return std::nullopt;
    return *r;
}

} // namespace


std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    subs.push_back(nats.queue_subscribe(
        "refdata.v1.>", "ores.refdata.service",
        [&nats, base_ctx = ctx, verifier](ores::nats::message msg) mutable {
            const auto ctx = ores::service::service::make_request_context(base_ctx, msg, verifier);
            const auto& subj = msg.subject;

            // ----------------------------------------------------------------
            // Countries
            // ----------------------------------------------------------------
            if (subj.ends_with(".countries.list")) {
                service::country_service svc(ctx);
                get_countries_response resp;
                try {
                    if (auto req = decode<get_countries_request>(msg)) {
                        resp.countries = svc.list_countries(
                            static_cast<std::uint32_t>(req->offset),
                            static_cast<std::uint32_t>(req->limit));
                        resp.total_available_count =
                            static_cast<int>(svc.count_countries());
                    }
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".countries.save")) {
                service::country_service svc(ctx);
                if (auto req = decode<save_country_request>(msg)) {
                    try {
                        svc.save_country(req->data);
                        reply(nats, msg,
                            save_country_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_country_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".countries.delete")) {
                service::country_service svc(ctx);
                if (auto req = decode<delete_country_request>(msg)) {
                    try {
                        svc.delete_countries(req->alpha2_codes);
                        reply(nats, msg,
                            delete_country_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_country_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".countries.history")) {
                service::country_service svc(ctx);
                if (auto req = decode<get_country_history_request>(msg)) {
                    try {
                        auto history = svc.get_country_history(req->alpha2_code);
                        reply(nats, msg, get_country_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_country_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Currencies
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".currencies.list")) {
                service::currency_service svc(ctx);
                get_currencies_response resp;
                try {
                    if (auto req = decode<get_currencies_request>(msg)) {
                        resp.currencies = svc.list_currencies(
                            static_cast<std::uint32_t>(req->offset),
                            static_cast<std::uint32_t>(req->limit));
                        resp.total_available_count =
                            static_cast<int>(svc.count_currencies());
                    }
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".currencies.save")) {
                service::currency_service svc(ctx);
                if (auto req = decode<save_currency_request>(msg)) {
                    try {
                        svc.save_currency(req->data);
                        reply(nats, msg,
                            save_currency_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_currency_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".currencies.delete")) {
                service::currency_service svc(ctx);
                if (auto req = decode<delete_currency_request>(msg)) {
                    try {
                        svc.delete_currencies(req->iso_codes);
                        reply(nats, msg,
                            delete_currency_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_currency_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Parties
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".parties.list")) {
                service::party_service svc(ctx);
                get_parties_response resp;
                try {
                    if (auto req = decode<get_parties_request>(msg)) {
                        resp.parties = svc.list_parties(
                            static_cast<std::uint32_t>(req->offset),
                            static_cast<std::uint32_t>(req->limit));
                        resp.total_available_count =
                            static_cast<int>(svc.count_parties());
                    }
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".parties.save")) {
                service::party_service svc(ctx);
                if (auto req = decode<save_party_request>(msg)) {
                    try {
                        svc.save_party(req->data);
                        reply(nats, msg,
                            save_party_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_party_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".parties.delete")) {
                service::party_service svc(ctx);
                if (auto req = decode<delete_party_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        for (const auto& id_str : req->ids)
                            svc.remove_party(gen(id_str));
                        reply(nats, msg,
                            delete_party_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_party_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".parties.history")) {
                service::party_service svc(ctx);
                if (auto req = decode<get_party_history_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        auto history = svc.get_party_history(gen(req->id));
                        reply(nats, msg, get_party_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_party_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Counterparties
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".counterparties.list")) {
                service::counterparty_service svc(ctx);
                get_counterparties_response resp;
                try {
                    if (auto req = decode<get_counterparties_request>(msg)) {
                        resp.counterparties = svc.list_counterparties(
                            static_cast<std::uint32_t>(req->offset),
                            static_cast<std::uint32_t>(req->limit));
                        resp.total_available_count =
                            static_cast<int>(svc.count_counterparties());
                    }
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".counterparties.save")) {
                service::counterparty_service svc(ctx);
                if (auto req = decode<save_counterparty_request>(msg)) {
                    try {
                        svc.save_counterparty(req->data);
                        reply(nats, msg,
                            save_counterparty_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_counterparty_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".counterparties.delete")) {
                service::counterparty_service svc(ctx);
                if (auto req = decode<delete_counterparty_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        for (const auto& id_str : req->ids)
                            svc.remove_counterparty(gen(id_str));
                        reply(nats, msg,
                            delete_counterparty_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_counterparty_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".counterparties.history")) {
                service::counterparty_service svc(ctx);
                if (auto req = decode<get_counterparty_history_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        auto history =
                            svc.get_counterparty_history(gen(req->id));
                        reply(nats, msg, get_counterparty_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_counterparty_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Books
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".books.list")) {
                service::book_service svc(ctx);
                get_books_response resp;
                try {
                    resp.books = svc.list_books();
                    resp.total_available_count =
                        static_cast<int>(resp.books.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".books.save")) {
                service::book_service svc(ctx);
                if (auto req = decode<save_book_request>(msg)) {
                    try {
                        svc.save_book(req->data);
                        reply(nats, msg,
                            save_book_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_book_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".books.delete")) {
                service::book_service svc(ctx);
                if (auto req = decode<delete_book_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        for (const auto& id_str : req->ids)
                            svc.remove_book(gen(id_str));
                        reply(nats, msg,
                            delete_book_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_book_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".books.history")) {
                service::book_service svc(ctx);
                if (auto req = decode<get_book_history_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        auto history = svc.get_book_history(gen(req->id));
                        reply(nats, msg, get_book_history_response{
                            .success = true,
                            .versions = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_book_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Portfolios
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".portfolios.list")) {
                service::portfolio_service svc(ctx);
                get_portfolios_response resp;
                try {
                    resp.portfolios = svc.list_portfolios();
                    resp.total_available_count =
                        static_cast<int>(resp.portfolios.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".portfolios.save")) {
                service::portfolio_service svc(ctx);
                if (auto req = decode<save_portfolio_request>(msg)) {
                    try {
                        svc.save_portfolio(req->data);
                        reply(nats, msg,
                            save_portfolio_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_portfolio_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".portfolios.delete")) {
                service::portfolio_service svc(ctx);
                if (auto req = decode<delete_portfolio_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        for (const auto& id_str : req->ids)
                            svc.remove_portfolio(gen(id_str));
                        reply(nats, msg,
                            delete_portfolio_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_portfolio_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".portfolios.history")) {
                service::portfolio_service svc(ctx);
                if (auto req = decode<get_portfolio_history_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        auto history =
                            svc.get_portfolio_history(gen(req->id));
                        reply(nats, msg, get_portfolio_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_portfolio_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Business units
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".business-units.list")) {
                service::business_unit_service svc(ctx);
                get_business_units_response resp;
                try {
                    resp.business_units = svc.list_business_units();
                    resp.total_available_count =
                        static_cast<int>(resp.business_units.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".business-units.save")) {
                service::business_unit_service svc(ctx);
                if (auto req = decode<save_business_unit_request>(msg)) {
                    try {
                        svc.save_business_unit(req->data);
                        reply(nats, msg,
                            save_business_unit_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_business_unit_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".business-units.delete")) {
                service::business_unit_service svc(ctx);
                if (auto req = decode<delete_business_unit_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        for (const auto& id_str : req->ids)
                            svc.remove_business_unit(gen(id_str));
                        reply(nats, msg,
                            delete_business_unit_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_business_unit_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".business-units.history")) {
                service::business_unit_service svc(ctx);
                if (auto req = decode<get_business_unit_history_request>(msg)) {
                    try {
                        boost::uuids::string_generator gen;
                        auto history =
                            svc.get_business_unit_history(gen(req->id));
                        reply(nats, msg, get_business_unit_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_business_unit_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Contact types
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".contact-types.list")) {
                service::contact_type_service svc(ctx);
                get_contact_types_response resp;
                try {
                    resp.contact_types = svc.list_types();
                    resp.total_available_count =
                        static_cast<int>(resp.contact_types.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".contact-types.save")) {
                service::contact_type_service svc(ctx);
                if (auto req = decode<save_contact_type_request>(msg)) {
                    try {
                        svc.save_type(req->data);
                        reply(nats, msg,
                            save_contact_type_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_contact_type_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".contact-types.delete")) {
                service::contact_type_service svc(ctx);
                if (auto req = decode<delete_contact_type_request>(msg)) {
                    try {
                        svc.remove_type(req->type);
                        reply(nats, msg,
                            delete_contact_type_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_contact_type_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".contact-types.history")) {
                service::contact_type_service svc(ctx);
                if (auto req = decode<get_contact_type_history_request>(msg)) {
                    try {
                        auto history = svc.get_type_history(req->type);
                        reply(nats, msg, get_contact_type_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_contact_type_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Monetary natures
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".monetary-natures.list")) {
                service::monetary_nature_service svc(ctx);
                get_monetary_natures_response resp;
                try {
                    resp.monetary_natures = svc.list_types();
                    resp.total_available_count =
                        static_cast<int>(resp.monetary_natures.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".monetary-natures.save")) {
                service::monetary_nature_service svc(ctx);
                if (auto req = decode<save_monetary_nature_request>(msg)) {
                    try {
                        svc.save_type(req->data);
                        reply(nats, msg,
                            save_monetary_nature_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_monetary_nature_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".monetary-natures.delete")) {
                service::monetary_nature_service svc(ctx);
                if (auto req = decode<delete_monetary_nature_request>(msg)) {
                    try {
                        svc.remove_type(req->nature);
                        reply(nats, msg,
                            delete_monetary_nature_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_monetary_nature_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".monetary-natures.history")) {
                service::monetary_nature_service svc(ctx);
                if (auto req = decode<get_monetary_nature_history_request>(msg)) {
                    try {
                        auto history = svc.get_type_history(req->nature);
                        reply(nats, msg, get_monetary_nature_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_monetary_nature_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Rounding types
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".rounding-types.list")) {
                service::rounding_type_service svc(ctx);
                get_rounding_types_response resp;
                try {
                    resp.rounding_types = svc.list_types();
                    resp.total_available_count =
                        static_cast<int>(resp.rounding_types.size());
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".rounding-types.save")) {
                service::rounding_type_service svc(ctx);
                if (auto req = decode<save_rounding_type_request>(msg)) {
                    try {
                        svc.save_type(req->data);
                        reply(nats, msg,
                            save_rounding_type_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_rounding_type_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".rounding-types.delete")) {
                service::rounding_type_service svc(ctx);
                if (auto req = decode<delete_rounding_type_request>(msg)) {
                    try {
                        svc.remove_type(req->type);
                        reply(nats, msg,
                            delete_rounding_type_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_rounding_type_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".rounding-types.history")) {
                service::rounding_type_service svc(ctx);
                if (auto req = decode<get_rounding_type_history_request>(msg)) {
                    try {
                        auto history = svc.get_type_history(req->type);
                        reply(nats, msg, get_rounding_type_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_rounding_type_history_response{
                            .success = false, .message = e.what()});
                    }
                }

            // ----------------------------------------------------------------
            // Business centres
            // ----------------------------------------------------------------
            } else if (subj.ends_with(".business-centres.list")) {
                service::business_centre_service svc(ctx);
                get_business_centres_response resp;
                try {
                    if (auto req = decode<get_business_centres_request>(msg)) {
                        resp.business_centres = svc.list_business_centres(
                            static_cast<std::uint32_t>(req->offset),
                            static_cast<std::uint32_t>(req->limit));
                        resp.total_available_count =
                            static_cast<int>(svc.count_business_centres());
                    }
                } catch (...) {}
                reply(nats, msg, resp);

            } else if (subj.ends_with(".business-centres.save")) {
                service::business_centre_service svc(ctx);
                if (auto req = decode<save_business_centre_request>(msg)) {
                    try {
                        svc.save_business_centre(req->data);
                        reply(nats, msg,
                            save_business_centre_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_business_centre_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".business-centres.delete")) {
                service::business_centre_service svc(ctx);
                if (auto req = decode<delete_business_centre_request>(msg)) {
                    try {
                        svc.delete_business_centres(req->codes);
                        reply(nats, msg,
                            delete_business_centre_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_business_centre_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".business-centres.history")) {
                service::business_centre_service svc(ctx);
                if (auto req =
                        decode<get_business_centre_history_request>(msg)) {
                    try {
                        auto history =
                            svc.get_business_centre_history(req->code);
                        reply(nats, msg, get_business_centre_history_response{
                            .success = true,
                            .history = std::move(history)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_business_centre_history_response{
                            .success = false, .message = e.what()});
                    }
                }
            }
        }));

    return subs;
}

}
