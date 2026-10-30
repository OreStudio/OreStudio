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
#include "ores.marketdata.service/app/curve_notices.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.inbox.core/service/notification_center.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

using namespace ores::logging;

namespace {

auto& notices_lg() {
    static auto instance = ores::logging::make_logger("ores.marketdata.service.curve_notices");
    return instance;
}

/**
 * @brief The permission an account reads a curve's values under.
 *
 * The decision names "the roles that price from the curve" as a recipient class
 * of their own. Roles are not permissions and a notification names an account,
 * so the nearest thing the model carries is the permission every reader of a
 * curve holds: the series read. The people who hold it are resolved when the
 * notice is raised, as every other audience of a notice is.
 */
constexpr std::string_view curve_reader_permission = "marketdata::market_series:read";

/**
 * @brief The characters a curve's URI carries that a path may not.
 *
 * The same escaping a browser applies to a path segment, so the record page reads
 * the URI back as it was written.
 */
std::string escape_path(const std::string& value) {
    static constexpr char digits[] = "0123456789ABCDEF";
    std::string out;
    out.reserve(value.size());
    for (const unsigned char c : value) {
        const bool plain = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                           (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.' || c == '~';
        if (plain) {
            out.push_back(static_cast<char>(c));
        } else {
            out.push_back('%');
            out.push_back(digits[c >> 4]);
            out.push_back(digits[c & 0x0F]);
        }
    }
    return out;
}

/**
 * @brief Where a notice points: the published curve's record page.
 *
 * The record page is named by the curve, which is the series' oresmd URI: the
 * URI is the only name a curve has, and it carries characters a path cannot, so
 * it is escaped once here. The bell prefixes the slash, so the route is stated
 * without one.
 */
std::string curve_notice_name_of(const ores::database::context& ctx,
                                 const domain::curve_publish& record) {
    repository::market_series_repository repo;
    const auto series = repo.read_latest(ctx, boost::uuids::to_string(record.output_series_id));
    return series.empty() ? "series " + boost::uuids::to_string(record.output_series_id) :
                            series.front().oresmd_uri;
}

std::string record_link(const std::string& curve_name) {
    return "market-data/published-curves/" + escape_path(curve_name);
}

/**
 * @brief The party the curve belongs to, as a person reads it.
 */
std::string party_name_of(const ores::database::context& ctx, const boost::uuids::uuid& party_id) {
    if (party_id.is_nil())
        return {};
    ores::refdata::repository::party_repository repo;
    const auto parties = repo.read_latest(ctx, boost::uuids::to_string(party_id));
    if (parties.empty())
        return boost::uuids::to_string(party_id);
    return parties.front().short_code;
}

/**
 * @brief The people who read this curve.
 *
 * Two of the three classes the decision names are reachable:
 *  - the accounts subscribed through their party's market data subscription,
 *    which the model carries as the accounts associated with the party that
 *    holds the curve;
 *  - the accounts that hold the permission a curve is read under, which is the
 *    nearest thing the model has to "the roles that price from the curve": a
 *    role is a bundle of permissions and a notification names an account.
 *
 * The third class, the services that consume the curve, is not reachable: a
 * recipient row names an account of the notice's own tenant, and the services
 * are the system tenant's accounts, so a tenant's notice cannot be addressed to
 * one. They read the change of the record instead, which the publish table's own
 * notify trigger writes.
 */
std::vector<std::string> readers_of(const ores::database::context& ctx,
                                    const boost::uuids::uuid& party_id) {
    std::set<std::string> readers;

    if (!party_id.is_nil()) {
        const auto rows = ores::database::repository::execute_parameterized_string_query(
            ctx,
            "select distinct a.id::text"
            " from ores_iam_account_parties_tbl ap"
            " join ores_iam_accounts_tbl a on a.id = ap.account_id"
            "   and a.tenant_id = ap.tenant_id"
            "   and a.valid_to = ores_utility_infinity_timestamp_fn()"
            " where ap.party_id = $1::uuid"
            "   and ap.tenant_id = ores_iam_current_tenant_id_fn()"
            "   and ap.valid_to = ores_utility_infinity_timestamp_fn()"
            "   and a.account_type = 'user'",
            {boost::uuids::to_string(party_id)},
            notices_lg(),
            "Reading the accounts of the party the curve belongs to");
        readers.insert(rows.begin(), rows.end());
    }

    try {
        ores::inbox::service::notification_center center(ctx);
        for (const auto& holder : center.holders_of(std::string(curve_reader_permission)))
            readers.insert(holder);
    } catch (const std::exception& e) {
        // A notice reaches whoever it can: the party's accounts are already
        // resolved, and a failure to add the permission's holders loses no
        // reader that is owed the notice by name.
        BOOST_LOG_SEV(notices_lg(), warn)
            << "The holders of " << curve_reader_permission << " were not read: " << e.what();
    }

    return {readers.begin(), readers.end()};
}

/**
 * @brief Raises the notice, or logs why it could not be raised.
 *
 * The raiser is the account that applied the act, because the notice says who
 * moved the set and a person reads it. A context with no account -- a sweep, a
 * replayed write -- falls back to the service's own.
 */
void raise_notice(const ores::database::context& ctx,
                  std::string_view kind_code,
                  const boost::uuids::uuid& party_id,
                  std::vector<ores::inbox::messaging::notification_argument_value> arguments,
                  const std::string& link_id) {
    try {
        ores::inbox::service::notification_center center(ctx);
        const auto raiser = center.actor_account_id();
        if (!raiser)
            throw std::runtime_error("the account that acted was not found");

        const auto readers = readers_of(ctx, party_id);
        if (readers.empty())
            // Nobody is subscribed: the queue and nobody else, as the decision
            // states. The read still states the new instant.
            return;

        ores::inbox::messaging::raise_notification_request n{.kind_code = std::string(kind_code),
                                                             .link_route = link_id,
                                                             .link_id = "",
                                                             .arguments = std::move(arguments),
                                                             .account_ids = {},
                                                             .audience_permission_code = ""};
        const auto result = center.raise(n, readers, *raiser);
        BOOST_LOG_SEV(notices_lg(), info)
            << "Raised " << kind_code << " notice " << result.notification_id << " for "
            << result.recipient_count << " reader(s)";
    } catch (const std::exception& e) {
        // Telling is never the act: the set moved, and the record says so.
        BOOST_LOG_SEV(notices_lg(), warn)
            << "A " << kind_code << " notice was not raised: " << e.what();
    }
}

std::string instant_of(const std::optional<std::chrono::system_clock::time_point>& at) {
    if (!at)
        return {};
    return ores::platform::time::datetime::to_iso8601_utc(*at);
}

}

void announce_publish(const ores::database::context& ctx,
                      const domain::curve_publish& published,
                      std::optional<std::chrono::system_clock::time_point> previous_as_of) {
    const auto curve = curve_notice_name_of(ctx, published);
    const auto curve_link = record_link(curve);
    raise_notice(ctx,
                 curve_published_kind_code,
                 published.party_id,
                 {{.name = "curve", .value = curve},
                  {.name = "party", .value = party_name_of(ctx, published.party_id)},
                  {.name = "as_of", .value = instant_of(published.as_of)},
                  {.name = "published_at", .value = instant_of(published.published_at)},
                  {.name = "publisher", .value = published.published_by},
                  {.name = "request", .value = boost::uuids::to_string(published.request_id)},
                  {.name = "previous_as_of", .value = instant_of(previous_as_of)}},
                 curve_link);
}

void announce_withdrawal(const ores::database::context& ctx,
                         const domain::curve_publish& withdrawn,
                         std::optional<std::chrono::system_clock::time_point> previous_as_of) {
    const auto curve = curve_notice_name_of(ctx, withdrawn);
    const auto curve_link = record_link(curve);
    raise_notice(ctx,
                 curve_withdrawn_kind_code,
                 withdrawn.party_id,
                 {{.name = "curve", .value = curve},
                  {.name = "party", .value = party_name_of(ctx, withdrawn.party_id)},
                  {.name = "as_of", .value = instant_of(previous_as_of)},
                  {.name = "published_by", .value = withdrawn.published_by},
                  {.name = "request", .value = boost::uuids::to_string(withdrawn.request_id)},
                  {.name = "withdrawn_at", .value = instant_of(withdrawn.withdrawn_at)},
                  {.name = "withdrawn_by", .value = withdrawn.withdrawn_by},
                  {.name = "reason", .value = withdrawn.withdrawn_reason_code}},
                 curve_link);
}

}
