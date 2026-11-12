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
#include "ores.http/routes/synthetic/calibration_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.synthetic.api/messaging/calibration_policy_protocol.hpp"
#include "ores.synthetic.api/messaging/calibration_provider_order_protocol.hpp"
#include "ores.synthetic.api/messaging/feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_fixing_item_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <cstdint>
#include <limits>
#include <map>
#include <optional>
#include <regex>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::http::routes::synthetic {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
using ores::utility::domain::change_intent;
using ores::utility::domain::precondition_kind;
namespace forwarding = ores::http::forwarding;
namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace synthetic_domain = ores::synthetic::domain;
namespace synthetic_messaging = ores::synthetic::messaging;

namespace {

/// The bodies the calibration screen sends, beside the wire_fields each is read with.
namespace calibration {

/// A run of every feed of the class, or of the one feed it names.
struct run_body final {
    std::optional<std::string> configId;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<run_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"configId", &run_body::configId}};
}

/// A reviewer's decision on a proposed variance calibration, and why.
struct review_body final {
    std::string decision;
    std::string reason;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<review_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"decision", &review_body::decision},
                                              field{"reason", &review_body::reason}};
}

/// The policy a person states for a class, at the version it was read, or null when new.
struct policy_body final {
    rr::stated_intent intent;
    std::optional<std::int64_t> version;
    std::int64_t window_days = 0;
    bool allow_inverse = false;
    std::string fallback_code;
    std::vector<std::string> provider_order;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<policy_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"intent", &policy_body::intent},
        field{"version", &policy_body::version},
        field{"window_days", &policy_body::window_days},
        field{"allow_inverse", &policy_body::allow_inverse},
        field{"fallback_code", &policy_body::fallback_code},
        field{"provider_order", &policy_body::provider_order}};
}

/// One candidate a person pins on a feed, read over the window given.
struct pin_body final {
    rr::stated_intent intent;
    std::string item;
    std::string provider_code;
    std::int64_t window_days = 0;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<pin_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"intent", &pin_body::intent},
                                              field{"item", &pin_body::item},
                                              field{"provider_code", &pin_body::provider_code},
                                              field{"window_days", &pin_body::window_days}};
}

/// A removal, which states only why it is made.
struct intent_body final {
    rr::stated_intent intent;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<intent_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"intent", &intent_body::intent}};
}

}

const std::string intent_sentence =
    "A write states a reason of 1 to 200 characters and commentary of up to 2000.";

/// Whether a text is 1 to most lower case letters, digits or underscores.
bool is_lower_code(std::string_view text, std::size_t most) {
    return !text.empty() && text.size() <= most && std::ranges::all_of(text, [](char c) {
        return (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_';
    });
}

/// The asset class the address names, as its refdata code.
std::string asset_class_of(const http_request& req) {
    auto code = rr::trimmed(req.get_path_param("assetClass"));
    if (!is_lower_code(code, 40))
        rr::invalid("An asset class is named by its code: 1 to 40 lower case letters, digits or "
                    "underscores.");
    return code;
}

/// The policy and its reviews are the installation's: only the system tenant changes them.
void require_system(const http_request& req) {
    const auto& user = req.authenticated_user;
    if (!user || !user->tenant_id || *user->tenant_id != rr::system_tenant_id)
        rr::refuse(http_status::forbidden,
                   "forbidden",
                   "A calibration policy belongs to the installation, so only the system "
                   "tenant changes it.");
}

/// The body read as its shape, or a refusal naming what it must state.
template <typename Body>
Body calibration_body_of(const http_request& req, const std::string& sentence) {
    auto body = rr::read_body<Body>(req.body);
    if (!body)
        rr::invalid(sentence);
    return std::move(*body);
}

/// The intent a write states, checked.
change_intent intent_from(const rr::stated_intent& stated) {
    auto intent = rr::intent_of(stated);
    if (!intent)
        rr::invalid(intent_sentence);
    return std::move(*intent);
}

/// A whole number of days from 2 to 180, the window a calibration reads over.
int window_of(std::int64_t days, const std::string& sentence) {
    if (days < 2 || days > 180)
        rr::invalid(sentence);
    return static_cast<int>(days);
}

/// The class's policy row, when it has one.
std::optional<synthetic_domain::calibration_policy>
policy_row_of(const http_request& req, nats_client& session, const std::string& asset_class) {
    const auto rows =
        forwarding::forward_all(req,
                                session,
                                synthetic_messaging::list_calibration_policies_request{},
                                &synthetic_messaging::list_calibration_policies_response::policies);
    const auto found = std::ranges::find(
        rows, asset_class, &synthetic_domain::calibration_policy::asset_class_code);
    if (found == rows.end())
        return std::nullopt;
    return *found;
}

/// The policy's provider order, its rungs in rank order.
std::vector<synthetic_domain::calibration_provider_order>
order_rows_of(const http_request& req, nats_client& session, const boost::uuids::uuid& policy_id) {
    synthetic_messaging::list_calibration_provider_orders_request msg;
    msg.filter.emplace();
    msg.filter->policy_id = policy_id;
    auto rows = forwarding::forward_all(
        req,
        session,
        msg,
        &synthetic_messaging::list_calibration_provider_orders_response::provider_orders);
    std::ranges::sort(rows, {}, &synthetic_domain::calibration_provider_order::rank);
    return rows;
}

void remove_orders(const http_request& req,
                   nats_client& session,
                   const change_intent& intent,
                   const std::vector<synthetic_domain::calibration_provider_order>& rows) {
    if (rows.empty())
        return;
    synthetic_messaging::delete_many_calibration_provider_orders_request msg;
    msg.intent = intent;
    for (const auto& row : rows)
        msg.removals.push_back({.key = {.id = row.id}});
    rr::require_ok(forwarding::forward(req, session, msg).result);
}

/// The items a feed names, when it names its own instead of deriving them from its policy.
std::vector<synthetic_domain::fx_spot_fixing_item>
item_rows_of(const http_request& req, nats_client& session, const boost::uuids::uuid& config_id) {
    synthetic_messaging::list_fx_spot_fixing_items_request msg;
    msg.filter.emplace();
    msg.filter->fx_spot_config_id = config_id;
    return forwarding::forward_all(
        req,
        session,
        msg,
        &synthetic_messaging::list_fx_spot_fixing_items_response::fx_spot_fixing_items);
}

void remove_items(const http_request& req,
                  nats_client& session,
                  const change_intent& intent,
                  const std::vector<synthetic_domain::fx_spot_fixing_item>& rows) {
    if (rows.empty())
        return;
    synthetic_messaging::delete_many_fx_spot_fixing_items_request msg;
    msg.intent = intent;
    for (const auto& row : rows)
        msg.removals.push_back({.key = {.id = row.id}});
    rr::require_ok(forwarding::forward(req, session, msg).result);
}

/// The feed the address names.
boost::uuids::uuid feed_of(const http_request& req) {
    const auto id = req.get_path_param("id");
    if (!rr::is_uuid(id))
        rr::invalid("A feed is named by its identifier.");
    return boost::uuids::string_generator()(id);
}

/// The policy a body states, checked: the order names each provider once.
struct stated_policy final {
    change_intent intent;
    std::optional<std::uint32_t> version;
    int window_days = 0;
    bool allow_inverse = false;
    std::string fallback_code;
    std::vector<std::string> provider_order;
};

stated_policy policy_of(const http_request& req) {
    const std::string sentence =
        "A policy states an intent, the version it was read at or null, a window of 2 to 180 "
        "days, whether a pair may be inverted, a fallback of keep_fixed or refuse, and up to 50 "
        "providers of up to 100 characters each.";
    const auto body = calibration_body_of<calibration::policy_body>(req, sentence);
    stated_policy out;
    out.intent = intent_from(body.intent);
    if (body.version) {
        if (*body.version < 0 || *body.version > std::numeric_limits<std::uint32_t>::max())
            rr::invalid(sentence);
        out.version = static_cast<std::uint32_t>(*body.version);
    }
    out.window_days = window_of(body.window_days, sentence);
    out.allow_inverse = body.allow_inverse;
    if (body.fallback_code != "keep_fixed" && body.fallback_code != "refuse")
        rr::invalid(sentence);
    out.fallback_code = body.fallback_code;
    if (body.provider_order.size() > 50)
        rr::invalid(sentence);
    std::set<std::string, std::less<>> named;
    for (const auto& raw : body.provider_order) {
        auto provider = rr::trimmed(raw);
        if (provider.empty() || rr::characters_of(provider) > 100)
            rr::invalid(sentence);
        if (!named.insert(provider).second)
            rr::invalid("The provider order names a provider twice.");
        out.provider_order.push_back(std::move(provider));
    }
    return out;
}

/// The candidate a pin states, checked.
struct stated_pin final {
    change_intent intent;
    std::string item;
    std::string provider_code;
    int window_days = 0;
};

stated_pin pin_of(const http_request& req) {
    const std::string sentence =
        "A pin states an intent, an FX fixing item of up to 500 characters, a provider code of "
        "up to 100 lower case letters, digits or underscores, and a window of 2 to 180 days.";
    static const std::regex fixing_item(R"(^oresmd://fx/[A-Z]{3}\?(?:.*&)?type=fixing(?:&|$))");
    const auto body = calibration_body_of<calibration::pin_body>(req, sentence);
    stated_pin out;
    out.intent = intent_from(body.intent);
    out.item = rr::trimmed(body.item);
    if (out.item.size() > 500 || !std::regex_search(out.item, fixing_item))
        rr::invalid(sentence);
    out.provider_code = rr::trimmed(body.provider_code);
    if (!is_lower_code(out.provider_code, 100))
        rr::invalid(sentence);
    out.window_days = window_of(body.window_days, sentence);
    return out;
}

/// The refusals the review service states, and the HTTP answer each means.
const std::map<std::string, std::pair<http_status, std::string>, std::less<>> review_refusals{
    {"not_found", {http_status::not_found, "not-found"}},
    {"decided", {http_status::conflict, "conflict"}},
    {"invalid", {http_status::bad_request, "invalid-request"}},
};

}

asset_class_calibration
calibration_routes::calibration_of(std::string asset_class,
                                   synthetic_messaging::calibrate_response resolved,
                                   std::optional<int> policy_version,
                                   bool policy_writable) {
    asset_class_calibration out;
    out.assetClass = std::move(asset_class);
    const auto& p = resolved.policy;
    out.policy = {p.from_policy_row,
                  p.window_days,
                  p.variance_window_days,
                  p.variance_approval_mode,
                  p.variance_min_returns,
                  p.variance_require_starts_agree,
                  p.variance_min_persistence,
                  p.variance_max_persistence,
                  p.variance_min_reaction,
                  p.allow_inverse,
                  p.fallback_code,
                  p.provider_order,
                  policy_version};
    out.policyWritable = policy_writable;
    out.feeds = std::move(resolved.feeds);
    auto& c = out.coverage;
    c.total = static_cast<int>(out.feeds.size());
    for (const auto& feed : out.feeds) {
        if (feed.start.from_fixing)
            ++c.fromFixing;
        else if (!feed.refused)
            ++c.keptFixed;
        if (feed.refused)
            ++c.refused;
        if (feed.explicit_items || feed.window_days_override)
            ++c.overridden;
    }
    return out;
}

void calibration_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"synthetic"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/synthetic/calibration/{assetClass}")
            .summary("The calibration of an asset class")
            .description("A composed view owned by the synthetic service: the policy the class "
                         "calibrates under, every feed of it resolved as a start would, with its "
                         "candidates and their reasons, and the coverage counts. Nothing starts. "
                         "One resolution call, and a read of the policies for the row's version "
                         "when the class has one. Not cached, because a resolution reads the "
                         "newest fixings each time. The query may name one feed as configId.")
            .response<asset_class_calibration>()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::calibrate_request>([&] {
                    const auto asset_class = asset_class_of(req);
                    const auto config_id = req.get_query_param("configId");
                    if (!config_id.empty() && !rr::is_uuid(config_id))
                        rr::invalid("A calibration read may name one feed, as configId.");

                    synthetic_messaging::calibrate_request msg;
                    msg.asset_class_code = asset_class;
                    msg.config_id = config_id;
                    msg.apply = false;
                    auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   reply.message.empty() ?
                                       "The synthetic service refused the calibration read." :
                                       reply.message);
                    std::optional<int> version;
                    if (reply.policy.from_policy_row)
                        if (const auto row = policy_row_of(req, session, asset_class))
                            version = row->version;
                    const auto& user = req.authenticated_user;
                    const bool writable =
                        user && user->tenant_id && *user->tenant_id == rr::system_tenant_id;
                    return ores::http::net::view_answer(
                        req,
                        json::write(
                            calibration_of(asset_class, std::move(reply), version, writable)),
                        ores::http::net::view_cache_policy::not_stored());
                });
            }));

    add(router->post("/api/synthetic/calibration/{assetClass}/run")
            .summary("Calibrate an asset class's feeds and store what moved")
            .description("Each feed's resolved level and deviations are written as a new version "
                         "of its config, under the reason system.calibration, when they moved. "
                         "The body may name one feed, so a screen reports each feed as it is "
                         "done. A running feed takes the stored values at its next start.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::calibrate_request>([&] {
                    const auto asset_class = asset_class_of(req);
                    const std::string sentence =
                        "A calibration run may name one feed, as configId.";
                    std::optional<std::string> config_id;
                    if (!rr::trimmed(req.body).empty()) {
                        rr::refuse_unknown_members(req.body, {"configId"}, sentence);
                        config_id =
                            calibration_body_of<calibration::run_body>(req, sentence).configId;
                        if (config_id && !rr::is_uuid(*config_id))
                            rr::invalid(sentence);
                    }

                    synthetic_messaging::calibrate_request msg;
                    msg.asset_class_code = asset_class;
                    msg.config_id = config_id.value_or("");
                    msg.apply = true;
                    const auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success)
                        rr::refuse(http_status::bad_gateway, "upstream-unavailable", reply.message);
                    if (config_id && reply.feeds.empty())
                        rr::refuse(http_status::not_found,
                                   "not-found",
                                   "No feed " + *config_id + " calibrates in " + asset_class + ".");
                    return http_response::json("{\"assetClass\":" + json::write(asset_class) +
                                               ",\"feeds\":" + json::write(reply.feeds) + "}");
                });
            }));

    add(router->post("/api/synthetic/calibration/variance/{id}/review")
            .summary("Approve or reject one proposed variance calibration")
            .description("Approving supersedes the pair's earlier approval and points the pair's "
                         "GJR-GARCH feeds at the new calibration. A calibration that does not "
                         "exist answers 404, one already decided answers 409, and a malformed "
                         "review answers 400.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::review_variance_calibration_request>([&] {
                    require_system(req);
                    const auto id = req.get_path_param("id");
                    if (!rr::is_uuid(id))
                        rr::invalid("A variance calibration is named by its identifier.");
                    const std::string sentence =
                        "A review states a decision of approve or reject and a reason of 1 to "
                        "2000 characters.";
                    rr::refuse_unknown_members(req.body, {"decision", "reason"}, sentence);
                    const auto body = calibration_body_of<calibration::review_body>(req, sentence);
                    const auto reason = rr::trimmed(body.reason);
                    if ((body.decision != "approve" && body.decision != "reject") ||
                        reason.empty() || rr::characters_of(reason) > 2000)
                        rr::invalid(sentence);

                    synthetic_messaging::review_variance_calibration_request msg;
                    msg.calibration_id = id;
                    msg.decision = body.decision;
                    msg.reason = reason;
                    const auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success) {
                        const auto refused = review_refusals.find(reply.refusal);
                        if (refused == review_refusals.end())
                            rr::refuse(
                                http_status::bad_gateway, "upstream-unavailable", reply.message);
                        rr::refuse(refused->second.first, refused->second.second, reply.message);
                    }
                    return http_response::json(
                        "{\"state\":" + json::write(reply.state) +
                        ",\"supersededId\":" + json::write(reply.superseded_id) +
                        ",\"repointedFeeds\":" + json::write(reply.repointed_feeds) +
                        ",\"message\":" + json::write(reply.message) + "}");
                });
            }));

    add(router->put("/api/synthetic/calibration/{assetClass}/policy")
            .summary("State an asset class's calibration policy")
            .description("The policy row is written against the version read. The variance "
                         "window and approval rules are kept as stored, or as the defaults for a "
                         "class with no row. When the provider order differs from the stored "
                         "one, the stored rungs are removed and the list is written from rank 1. "
                         "Each write converges, so a save that fails part way is completed by "
                         "saving again.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::put_calibration_policy_request>([&] {
                    require_system(req);
                    const auto asset_class = asset_class_of(req);
                    const auto stated = policy_of(req);

                    const auto existing = policy_row_of(req, session, asset_class);
                    const auto kept = existing.value_or(synthetic_domain::calibration_policy{});
                    synthetic_messaging::calibration_policy_write write;
                    write.id = existing ? existing->id : boost::uuids::random_generator()();
                    write.asset_class_code = asset_class;
                    write.window_days = stated.window_days;
                    write.variance_window_days = kept.variance_window_days;
                    write.variance_approval_mode = kept.variance_approval_mode;
                    write.variance_min_returns = kept.variance_min_returns;
                    write.variance_require_starts_agree = kept.variance_require_starts_agree;
                    write.variance_min_persistence = kept.variance_min_persistence;
                    write.variance_max_persistence = kept.variance_max_persistence;
                    write.variance_min_reaction = kept.variance_min_reaction;
                    write.allow_inverse = stated.allow_inverse;
                    write.fallback_code = stated.fallback_code;
                    const auto policy_id = write.id;
                    rr::require_ok(
                        forwarding::put_at_version<
                            synthetic_messaging::put_calibration_policy_request>(
                            req, session, std::move(write), stated.version, stated.intent)
                            .result);

                    // A rank is unique within a policy, so rungs are not moved in place.
                    const auto orders =
                        existing ? order_rows_of(req, session, policy_id) :
                                   std::vector<synthetic_domain::calibration_provider_order>{};
                    if (std::ranges::equal(
                            orders,
                            stated.provider_order,
                            {},
                            &synthetic_domain::calibration_provider_order::provider_code))
                        return rr::no_content();
                    remove_orders(req, session, stated.intent, orders);
                    if (stated.provider_order.empty())
                        return rr::no_content();
                    synthetic_messaging::put_many_calibration_provider_orders_request add_order;
                    add_order.intent = stated.intent;
                    int rank = 0;
                    for (const auto& provider : stated.provider_order) {
                        synthetic_messaging::calibration_provider_order_change change;
                        change.write.id = boost::uuids::random_generator()();
                        change.write.policy_id = policy_id;
                        change.write.provider_code = provider;
                        change.write.rank = ++rank;
                        change.precondition.kind = precondition_kind::must_not_exist;
                        add_order.changes.push_back(std::move(change));
                    }
                    rr::require_ok(forwarding::forward(req, session, add_order).result);
                    return rr::no_content();
                });
            }));

    add(router->delete_("/api/synthetic/calibration/{assetClass}/policy")
            .summary("Return an asset class to automatic calibration")
            .description("The class's provider order and its policy row are removed. A class "
                         "with no policy row is already automatic.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::delete_calibration_policy_request>([&] {
                    require_system(req);
                    const auto asset_class = asset_class_of(req);
                    const auto intent = intent_from(
                        calibration_body_of<calibration::intent_body>(req, intent_sentence).intent);
                    const auto existing = policy_row_of(req, session, asset_class);
                    if (!existing)
                        return rr::no_content();
                    remove_orders(req, session, intent, order_rows_of(req, session, existing->id));
                    synthetic_messaging::delete_calibration_policy_request msg;
                    msg.intent = intent;
                    msg.removal.key.id = existing->id;
                    msg.removal.precondition = {precondition_kind::must_match_version,
                                                static_cast<std::uint32_t>(existing->version)};
                    rr::require_ok(forwarding::forward(req, session, msg).result);
                    return rr::no_content();
                });
            }));

    add(router->put("/api/synthetic/calibration/feeds/{id}/pin")
            .summary("Pin one candidate on a feed, so it overrides its class policy")
            .description("The feed's own items become the one item given, read on or before the "
                         "clock over the window given. The old items go first, so a write that "
                         "then fails leaves the feed following its policy, and pinning again "
                         "completes it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::put_many_fx_spot_fixing_items_request>([&] {
                    const auto config_id = feed_of(req);
                    const auto pin = pin_of(req);
                    remove_items(req, session, pin.intent, item_rows_of(req, session, config_id));
                    synthetic_messaging::put_many_fx_spot_fixing_items_request msg;
                    msg.intent = pin.intent;
                    synthetic_messaging::fx_spot_fixing_item_change change;
                    change.write.id = boost::uuids::random_generator()();
                    change.write.fx_spot_config_id = config_id;
                    change.write.rank = 1;
                    change.write.item = pin.item;
                    change.write.provider_code = pin.provider_code;
                    change.write.policy = "latest_on_or_before_clock";
                    change.write.window_days = pin.window_days;
                    change.precondition.kind = precondition_kind::must_not_exist;
                    msg.changes.push_back(std::move(change));
                    rr::require_ok(forwarding::forward(req, session, msg).result);
                    return rr::no_content();
                });
            }));

    add(router->delete_("/api/synthetic/calibration/feeds/{id}/pin")
            .summary("Clear a feed's own items, so it derives its candidates from its policy")
            .description("A feed with no items of its own is already following its policy.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::delete_many_fx_spot_fixing_items_request>(
                    [&] {
                        const auto config_id = feed_of(req);
                        const auto intent = intent_from(
                            calibration_body_of<calibration::intent_body>(req, intent_sentence)
                                .intent);
                        remove_items(req, session, intent, item_rows_of(req, session, config_id));
                        return rr::no_content();
                    });
            }));
}

}
