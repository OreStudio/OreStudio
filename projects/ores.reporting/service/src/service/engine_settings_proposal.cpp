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
#include "ores.reporting.service/service/engine_settings_proposal.hpp"
#include "ores.database/domain/context_transact.hpp"
#include "ores.database/pq/statement.hpp"
#include "ores.database/pq/transaction.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/service/approval_announcer.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.reporting.core/repository/engine_settings_building_repository.hpp"
#include "ores.reporting.core/repository/engine_settings_change_repository.hpp"
#include "ores.reporting.core/repository/engine_settings_execution_repository.hpp"
#include "ores.reporting.core/repository/engine_settings_fixings_repository.hpp"
#include "ores.reporting.core/repository/engine_settings_group_repository.hpp"
#include "ores.reporting.core/repository/engine_settings_group_setting_repository.hpp"
#include "ores.reporting.core/repository/engine_settings_repository.hpp"
#include "ores.reporting.core/repository/report_configuration_repository.hpp"
#include "ores.reporting.core/service/engine_settings_building_service.hpp"
#include "ores.reporting.core/service/engine_settings_execution_service.hpp"
#include "ores.reporting.core/service/engine_settings_fixings_service.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <algorithm>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <tuple>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace ores::reporting::service {

using namespace ores::logging;
namespace db = ores::database::repository;
using ores::utility::domain::outcome;

namespace {

constexpr std::string_view fixings_code = "fixings";
constexpr std::string_view building_code = "building";
constexpr std::string_view execution_code = "execution";

/** A proposed value of one setting, as the screen states it. */
using value_map = std::map<std::string, std::optional<std::string>>;

std::string trim(const std::string& s) {
    const auto first = s.find_first_not_of(" \t\r\n");
    if (first == std::string::npos)
        return {};
    const auto last = s.find_last_not_of(" \t\r\n");
    return s.substr(first, last - first + 1);
}

ores::utility::domain::result refusal_of(ores::utility::domain::outcome o, std::string message) {
    return {.outcome = o, .code = {}, .message = std::move(message), .fields = {}};
}

/** The text a setting's value in force is drawn as; empty for the engine default. */
std::string text_of(const std::optional<bool>& v) {
    if (!v)
        return {};
    return *v ? "true" : "false";
}

std::string text_of(const std::optional<int>& v) {
    if (!v)
        return {};
    return std::to_string(*v);
}

std::string text_of(const std::optional<std::string>& v) {
    return v.value_or(std::string{});
}

/** The engine default for a value the screen leaves empty. */
std::optional<bool> as_flag(const std::optional<std::string>& text) {
    const auto v = trim(text.value_or(std::string{}));
    if (v.empty())
        return std::nullopt;
    if (v == "true")
        return true;
    if (v == "false")
        return false;
    throw std::invalid_argument("A flag is true or false, not '" + v + "'.");
}

std::optional<int> as_whole(const std::optional<std::string>& text) {
    const auto v = trim(text.value_or(std::string{}));
    if (v.empty())
        return std::nullopt;
    std::size_t used = 0;
    const auto parsed = std::stoi(v, &used);
    if (used != v.size())
        throw std::invalid_argument("A whole is its digits, not '" + v + "'.");
    return parsed;
}

std::optional<std::string> as_choice(const std::optional<std::string>& text) {
    const auto v = trim(text.value_or(std::string{}));
    if (v.empty())
        return std::nullopt;
    return v;
}

/** The words a choice accepts, as the seed states them, comma separated. */
std::vector<std::string> words_of(const std::string& options) {
    std::vector<std::string> words;
    std::size_t start = 0;
    while (start <= options.size()) {
        const auto end = options.find(',', start);
        const auto word =
            trim(options.substr(start, end == std::string::npos ? std::string::npos : end - start));
        if (!word.empty())
            words.push_back(word);
        if (end == std::string::npos)
            break;
        start = end + 1;
    }
    return words;
}

/**
 * @brief Refuses a value the setting does not accept.
 *
 * The same check a direct write meets, made before a person is asked to decide
 * it: a proposal nobody could apply is a proposal nobody should sign.
 */
void validate(const domain::engine_settings_group_setting& setting,
              const std::optional<std::string>& text) {
    if (setting.kind == "flag") {
        as_flag(text);
        return;
    }
    if (setting.kind == "whole") {
        const auto parsed = as_whole(text);
        if (!parsed)
            return;
        if (setting.minimum && static_cast<double>(*parsed) < *setting.minimum)
            throw std::invalid_argument(setting.column_name + " is below its minimum.");
        if (setting.maximum && static_cast<double>(*parsed) > *setting.maximum)
            throw std::invalid_argument(setting.column_name + " is above its maximum.");
        return;
    }
    if (setting.kind == "choice") {
        const auto parsed = as_choice(text);
        if (!parsed)
            return;
        std::string allowed;
        bool found = false;
        for (const auto& word : words_of(setting.options)) {
            if (word == *parsed)
                found = true;
            allowed += (allowed.empty() ? "" : ", ") + word;
        }
        if (!found)
            throw std::invalid_argument(setting.column_name + " is not one of: " + allowed + ".");
        return;
    }
    throw std::invalid_argument("The setting " + setting.code + " has an unknown kind.");
}

/**
 * @brief The value in force of each setting of one group, by column.
 *
 * A group with no row answers every column empty, which is the engine default
 * for every setting in it.
 */
value_map values_in_force(const ores::database::context& ctx,
                          const std::string& group_code,
                          const boost::uuids::uuid& engine_settings_id,
                          const settings_group& group) {
    value_map values;
    for (const auto& setting : group.settings)
        values[setting.column_name] = std::nullopt;

    const auto id = boost::uuids::to_string(engine_settings_id);
    if (group_code == fixings_code) {
        const auto rows =
            repository::engine_settings_fixings_repository{}.read_latest_by_engine_settings_id(
                ctx, id, 0, 1000);
        if (rows.empty())
            return values;
        const auto& r = rows.front();
        values["imply_todays_fixings"] = text_of(r.imply_todays_fixings);
        values["include_todays_cash_flows"] = text_of(r.include_todays_cash_flows);
        values["include_reference_date_events"] = text_of(r.include_reference_date_events);
        values["enrich_index_fixings"] = text_of(r.enrich_index_fixings);
        values["ignore_fixing_lag"] = text_of(r.ignore_fixing_lag);
        values["ignore_fixing_lead"] = text_of(r.ignore_fixing_lead);
        return values;
    }
    if (group_code == building_code) {
        const auto rows =
            repository::engine_settings_building_repository{}.read_latest_by_engine_settings_id(
                ctx, id, 0, 1000);
        if (rows.empty())
            return values;
        const auto& r = rows.front();
        values["observation_model"] = text_of(r.observation_model);
        values["lazy_market_building"] = text_of(r.lazy_market_building);
        values["build_failed_trades"] = text_of(r.build_failed_trades);
        values["continue_on_error"] = text_of(r.continue_on_error);
        values["use_at_par_coupons_curves"] = text_of(r.use_at_par_coupons_curves);
        values["use_at_par_coupons_trades"] = text_of(r.use_at_par_coupons_trades);
        return values;
    }
    if (group_code == execution_code) {
        const auto rows =
            repository::engine_settings_execution_repository{}.read_latest_by_engine_settings_id(
                ctx, id, 0, 1000);
        if (rows.empty())
            return values;
        const auto& r = rows.front();
        values["n_threads"] = text_of(r.n_threads);
        values["log_mask"] = text_of(r.log_mask);
        values["gzip_compression_level"] = text_of(r.gzip_compression_level);
        return values;
    }
    return values;
}

/** The version of the group's row in force, or zero when the group has no row. */
int version_in_force(const ores::database::context& ctx,
                     const std::string& group_code,
                     const boost::uuids::uuid& engine_settings_id) {
    const auto id = boost::uuids::to_string(engine_settings_id);
    if (group_code == fixings_code) {
        const auto rows =
            repository::engine_settings_fixings_repository{}.read_latest_by_engine_settings_id(
                ctx, id, 0, 1000);
        return rows.empty() ? 0 : rows.front().version;
    }
    if (group_code == building_code) {
        const auto rows =
            repository::engine_settings_building_repository{}.read_latest_by_engine_settings_id(
                ctx, id, 0, 1000);
        return rows.empty() ? 0 : rows.front().version;
    }
    if (group_code == execution_code) {
        const auto rows =
            repository::engine_settings_execution_repository{}.read_latest_by_engine_settings_id(
                ctx, id, 0, 1000);
        return rows.empty() ? 0 : rows.front().version;
    }
    return 0;
}

/**
 * @brief Writes one group's part with the values a proposal holds.
 *
 * The row keeps the identity and the party of the row in force, so the write is
 * the next version of the section and not a second section. A group with no row
 * takes a new identity, which is the write that sets the group for the first
 * time.
 */
void write_group(const ores::database::context& ctx,
                 const std::string& group_code,
                 const boost::uuids::uuid& engine_settings_id,
                 const boost::uuids::uuid& party_id,
                 const value_map& values) {
    const auto id = boost::uuids::to_string(engine_settings_id);
    if (group_code == fixings_code) {
        const auto rows =
            repository::engine_settings_fixings_repository{}.read_latest_by_engine_settings_id(
                ctx, id, 0, 1000);
        domain::engine_settings_fixings row;
        if (!rows.empty()) {
            row = rows.front();
        } else {
            row.id = ores::utility::uuid::uuid_v7_generator{}();
            row.engine_settings_id = engine_settings_id;
            row.party_id = party_id;
            row.version = 0;
        }
        row.imply_todays_fixings = as_flag(values.at("imply_todays_fixings"));
        row.include_todays_cash_flows = as_flag(values.at("include_todays_cash_flows"));
        row.include_reference_date_events = as_flag(values.at("include_reference_date_events"));
        row.enrich_index_fixings = as_flag(values.at("enrich_index_fixings"));
        row.ignore_fixing_lag = as_whole(values.at("ignore_fixing_lag"));
        row.ignore_fixing_lead = as_whole(values.at("ignore_fixing_lead"));
        engine_settings_fixings_service(ctx).save_engine_settings_fixings(row);
        return;
    }
    if (group_code == building_code) {
        const auto rows =
            repository::engine_settings_building_repository{}.read_latest_by_engine_settings_id(
                ctx, id, 0, 1000);
        domain::engine_settings_building row;
        if (!rows.empty()) {
            row = rows.front();
        } else {
            row.id = ores::utility::uuid::uuid_v7_generator{}();
            row.engine_settings_id = engine_settings_id;
            row.party_id = party_id;
            row.version = 0;
        }
        row.observation_model = as_choice(values.at("observation_model"));
        row.lazy_market_building = as_flag(values.at("lazy_market_building"));
        row.build_failed_trades = as_flag(values.at("build_failed_trades"));
        row.continue_on_error = as_flag(values.at("continue_on_error"));
        row.use_at_par_coupons_curves = as_flag(values.at("use_at_par_coupons_curves"));
        row.use_at_par_coupons_trades = as_flag(values.at("use_at_par_coupons_trades"));
        engine_settings_building_service(ctx).save_engine_settings_building(row);
        return;
    }
    if (group_code == execution_code) {
        const auto rows =
            repository::engine_settings_execution_repository{}.read_latest_by_engine_settings_id(
                ctx, id, 0, 1000);
        domain::engine_settings_execution row;
        if (!rows.empty()) {
            row = rows.front();
        } else {
            row.id = ores::utility::uuid::uuid_v7_generator{}();
            row.engine_settings_id = engine_settings_id;
            row.party_id = party_id;
            row.version = 0;
        }
        row.n_threads = as_whole(values.at("n_threads"));
        row.log_mask = as_whole(values.at("log_mask"));
        row.gzip_compression_level = as_whole(values.at("gzip_compression_level"));
        engine_settings_execution_service(ctx).save_engine_settings_execution(row);
        return;
    }
    throw std::invalid_argument("No group of engine settings is called " + group_code + ".");
}

/** The change lines of a request, in group and setting order. */
std::vector<domain::engine_settings_change> lines_of(const ores::database::context& ctx,
                                                     const std::string& request_id) {
    auto rows = repository::engine_settings_change_repository{}.read_latest_by_request_id(
        ctx, request_id, 0, 1000);
    std::ranges::sort(rows, [](const auto& a, const auto& b) {
        return std::tie(a.group_code, a.setting_code) < std::tie(b.group_code, b.setting_code);
    });
    return rows;
}

/** The groups a request proposes, each once, in name order. */
std::vector<std::string> groups_of(const std::vector<domain::engine_settings_change>& rows) {
    std::vector<std::string> groups;
    for (const auto& r : rows) {
        if (std::ranges::find(groups, r.group_code) == groups.end())
            groups.push_back(r.group_code);
    }
    std::ranges::sort(groups);
    return groups;
}

/** The lines of one group, in setting order. */
std::vector<domain::engine_settings_change>
rows_of_group(const std::vector<domain::engine_settings_change>& rows,
              const std::string& group_code) {
    std::vector<domain::engine_settings_change> out;
    for (const auto& r : rows) {
        if (r.group_code == group_code)
            out.push_back(r);
    }
    return out;
}

/** The value one group of a request proposes, by column. */
value_map values_of(const std::vector<domain::engine_settings_change>& rows,
                    const settings_group& group) {
    value_map values;
    for (const auto& setting : group.settings) {
        values[setting.column_name] = std::nullopt;
        for (const auto& row : rows) {
            if (row.setting_code == setting.code) {
                values[setting.column_name] = row.text_value;
                break;
            }
        }
    }
    return values;
}

/**
 * @brief Locks the request row for the rest of the transaction.
 *
 * The id is parsed as a uuid and written back in its canonical form, so nothing
 * but a uuid reaches the statement text.
 */
void lock_request(const ores::database::context& ctx, const std::string& request_id) {
    const auto id = boost::uuids::to_string(boost::uuids::string_generator{}(request_id));
    const ores::database::pq::text_statement statement(
        {},
        "select id from ores_inbox_approval_requests_tbl where id = '" + id +
            "'::uuid and valid_to = ores_utility_infinity_timestamp_fn() for update");
    ores::database::transact(
        ctx, [&](ores::database::pq::transaction& tx) { tx.run(statement, {}); });
}

}

engine_settings_proposal_service::engine_settings_proposal_service(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<settings_group> engine_settings_proposal_service::groups() {
    const auto group_rows = repository::engine_settings_group_repository{}.read_latest(ctx_);
    const auto setting_rows =
        repository::engine_settings_group_setting_repository{}.read_latest(ctx_);

    auto ordered = group_rows;
    std::ranges::sort(ordered, {}, &domain::engine_settings_group::position);

    std::vector<settings_group> out;
    out.reserve(ordered.size());
    for (const auto& g : ordered) {
        settings_group group;
        group.code = g.code;
        for (const auto& s : setting_rows) {
            if (s.group_code == g.code)
                group.settings.push_back(s);
        }
        std::ranges::sort(group.settings, {}, &domain::engine_settings_group_setting::position);
        group.changes_numbers = std::ranges::any_of(group.settings, [](const auto& s) {
            return s.changes_numbers;
        });
        out.push_back(std::move(group));
    }
    return out;
}

std::optional<settings_group> engine_settings_proposal_service::group(const std::string& code) {
    for (auto& g : groups()) {
        if (g.code == code)
            return g;
    }
    return std::nullopt;
}

bool engine_settings_proposal_service::needs_approval(const boost::uuids::uuid& engine_settings_id,
                                                      const std::string& group_code) {
    const auto found = group(group_code);
    if (!found || !found->changes_numbers)
        return false;

    const auto settings = repository::engine_settings_repository{}.read_latest(
        ctx_, boost::uuids::to_string(engine_settings_id));
    if (settings.empty())
        return false;

    const auto bindings = repository::report_configuration_repository{}
                              .read_latest_by_configuration_id(
                                  ctx_,
                                  boost::uuids::to_string(settings.front().configuration_id),
                                  0,
                                  1000);
    return !bindings.empty();
}

std::vector<std::string> engine_settings_proposal_service::part_codes(const std::string& group_code) {
    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    const auto named = lifecycle.part_codes_of_kind(std::string(kind_code));
    if (std::ranges::find(named, group_code) != named.end())
        return {group_code};
    return {};
}

messaging::raise_engine_settings_change_response engine_settings_proposal_service::raise(
    const messaging::raise_engine_settings_change_request& request) {
    messaging::raise_engine_settings_change_response response;
    const auto asked = group(request.group_code);
    if (!asked) {
        response.result = refusal_of(
            outcome::invalid, "No group of engine settings is called " + request.group_code + ".");
        return response;
    }
    if (!asked->changes_numbers) {
        response.result = refusal_of(outcome::invalid,
                                     "The " + request.group_code +
                                         " group is operational, so a write to it lands directly "
                                         "and is not a proposal.");
        return response;
    }
    const auto settings = repository::engine_settings_repository{}.read_latest(
        ctx_, boost::uuids::to_string(request.engine_settings_id));
    if (settings.empty()) {
        response.result = refusal_of(outcome::missing, "No engine run settings have this id.");
        return response;
    }
    if (!needs_approval(request.engine_settings_id, request.group_code)) {
        response.result =
            refusal_of(outcome::invalid,
                       "No report binds these engine run settings, so they are a draft and the "
                       "group changes in place.");
        return response;
    }
    if (trim(request.reason).empty()) {
        response.result = refusal_of(outcome::invalid, "Say why you ask.");
        return response;
    }

    // The proposal states the whole group: a setting the person left alone
    // travels with the value in force, so the apply writes one group and not a
    // patch of it.
    auto values = values_in_force(ctx_, request.group_code, request.engine_settings_id, *asked);
    for (const auto& v : request.values) {
        const auto setting = std::ranges::find_if(asked->settings, [&](const auto& s) {
            return s.code == v.setting_code;
        });
        if (setting == asked->settings.end()) {
            response.result = refusal_of(outcome::invalid,
                                         v.setting_code + " is not a setting of the " +
                                             request.group_code + " group.");
            return response;
        }
        try {
            validate(*setting, v.text_value);
        } catch (const std::exception& e) {
            response.result = refusal_of(outcome::invalid, e.what());
            return response;
        }
        const auto text = trim(v.text_value);
        values[setting->column_name] = text.empty() ? std::nullopt : std::optional{text};
    }

    const auto in_force =
        values_in_force(ctx_, request.group_code, request.engine_settings_id, *asked);
    bool changes = false;
    for (const auto& [column, value] : values) {
        if (value != in_force.at(column))
            changes = true;
    }
    if (!changes) {
        response.result = refusal_of(outcome::invalid, "The proposal changes nothing.");
        return response;
    }

    const auto parts = part_codes(request.group_code);
    if (parts.empty()) {
        response.result =
            refusal_of(outcome::failed,
                       "The kind " + std::string(kind_code) + " names no part for the " +
                           request.group_code + " group, so nobody could sign it.");
        return response;
    }

    boost::uuids::uuid request_id{};
    {
        db::unit_of_work uow(ctx_);
        const auto& ctx = uow.ctx();
        ores::inbox::service::approval_lifecycle lifecycle(ctx);
        const auto kind = lifecycle.kind(std::string(kind_code));
        if (!kind)
            throw std::runtime_error("No such kind of request: " + std::string(kind_code));
        const auto me = lifecycle.actor_account_id();
        if (!me)
            throw std::runtime_error("The signed-in account was not found.");

        const auto raised = lifecycle.raise(*kind, request.reason, *me, parts);
        request_id = raised.id;

        std::vector<domain::engine_settings_change> rows;
        rows.reserve(asked->settings.size());
        for (const auto& setting : asked->settings) {
            domain::engine_settings_change row;
            row.id = ores::utility::uuid::uuid_v7_generator{}();
            row.tenant_id = ctx.tenant_id();
            row.request_id = raised.id;
            row.engine_settings_id = request.engine_settings_id;
            row.group_code = request.group_code;
            row.setting_code = setting.code;
            row.text_value = values.at(setting.column_name);
            row.base_version = request.base_version;
            row.applied = false;
            row.modified_by = ctx.actor();
            row.performed_by = ctx.service_account();
            row.change_reason_code = "system.new_record";
            row.change_commentary = request.reason;
            rows.push_back(std::move(row));
        }
        repository::engine_settings_change_repository{}.write(ctx, rows);
        uow.commit();
    }

    // The request stands whether or not anybody is told, so the telling happens
    // after the commit and a failure there is only logged.
    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    const auto kind = lifecycle.kind(std::string(kind_code));
    const auto raised = lifecycle.request(boost::uuids::to_string(request_id));
    if (kind && raised)
        ores::inbox::service::approval_announcer(ctx_).tell_open_parts(lifecycle, *kind, *raised);

    response.request_id = boost::uuids::to_string(request_id);
    response.result = refusal_of(outcome::ok, {});
    return response;
}

std::vector<messaging::engine_settings_change_line>
engine_settings_proposal_service::lines(const std::string& request_id) {
    const auto rows = lines_of(ctx_, request_id);

    std::vector<messaging::engine_settings_change_line> out;
    int line_no = 0;
    for (const auto& code : groups_of(rows)) {
        const auto group_rows = rows_of_group(rows, code);
        if (group_rows.empty())
            continue;
        const auto& first = group_rows.front();
        const auto settings = group(code);

        messaging::engine_settings_change_line line;
        line.line_no = ++line_no;
        line.group_code = code;
        line.engine_settings_id = first.engine_settings_id;
        line.base_version = first.base_version;
        line.applied = std::ranges::all_of(group_rows, [](const auto& r) { return r.applied; });

        if (settings) {
            const auto in_force =
                values_in_force(ctx_, code, first.engine_settings_id, *settings);
            for (const auto& setting : settings->settings) {
                messaging::engine_settings_value_change field;
                field.column_name = setting.column_name;
                field.ore_name = setting.ore_name;
                field.before = text_of(in_force.at(setting.column_name));
                for (const auto& row : group_rows) {
                    if (row.setting_code == setting.code) {
                        field.after = text_of(row.text_value);
                        break;
                    }
                }
                field.changed = field.before != field.after;
                line.fields.push_back(std::move(field));
            }
        }
        out.push_back(std::move(line));
    }
    return out;
}

std::vector<std::string> engine_settings_proposal_service::refusals(const std::string& request_id) {
    const auto rows = lines_of(ctx_, request_id);
    std::vector<std::string> out;
    for (const auto& code : groups_of(rows)) {
        const auto group_rows = rows_of_group(rows, code);
        if (group_rows.empty())
            continue;
        const auto in_force =
            version_in_force(ctx_, code, group_rows.front().engine_settings_id);
        if (in_force != group_rows.front().base_version) {
            out.push_back("The " + code + " group has moved since the proposal was raised: it is "
                          "at version " + std::to_string(in_force) + " and the proposal claims " +
                          std::to_string(group_rows.front().base_version) + ".");
            continue;
        }
        out.emplace_back();
    }
    return out;
}

engine_settings_apply engine_settings_proposal_service::apply(const std::string& request_id) {
    ores::inbox::service::approval_lifecycle lifecycle(ctx_);
    const auto request = lifecycle.request(request_id);
    if (!request)
        return {.refusal = "No such request."};
    if (request->state_code != "approved")
        return {.refusal = "The request is " + request->state_code + " and cannot be applied."};

    engine_settings_apply out;
    try {
        db::unit_of_work uow(ctx_);
        const auto& ctx = uow.ctx();

        // Two applies of one request must not both write, so the request row is
        // locked and its state is read again under the lock.
        lock_request(ctx, request_id);
        const auto locked = ores::inbox::service::approval_lifecycle(ctx).request(request_id);
        if (!locked || locked->state_code != "approved")
            return {.refusal = "The request is " +
                               (locked ? locked->state_code : std::string("gone")) +
                               " and cannot be applied."};

        const auto rows = lines_of(ctx, request_id);
        repository::engine_settings_change_repository changes;

        for (const auto& code : groups_of(rows)) {
            const auto group_rows = rows_of_group(rows, code);
            std::vector<domain::engine_settings_change> pending;
            for (const auto& r : group_rows) {
                if (!r.applied)
                    pending.push_back(r);
            }
            if (pending.empty())
                continue;

            const auto& first = pending.front();
            const auto in_force = version_in_force(ctx, code, first.engine_settings_id);
            if (in_force != first.base_version) {
                out.refusal = "The " + code + " group has moved since the proposal was raised.";
                break;
            }
            const auto settings = group(code);
            if (!settings) {
                out.refusal = "No group of engine settings is called " + code + ".";
                break;
            }

            const auto engine_settings = repository::engine_settings_repository{}.read_latest(
                ctx, boost::uuids::to_string(first.engine_settings_id));
            if (engine_settings.empty()) {
                out.refusal = "The engine run settings the proposal names are gone.";
                break;
            }

            write_group(ctx,
                        code,
                        first.engine_settings_id,
                        engine_settings.front().party_id,
                        values_of(pending, *settings));
            ++out.groups_applied;

            for (auto row : pending) {
                row.applied = true;
                row.modified_by = ctx.actor();
                row.performed_by = ctx.service_account();
                row.change_reason_code = "system.update";
                row.change_commentary = "Applied approval request " + request_id;
                changes.write(ctx, row);
            }
        }

        if (out.refusal.empty()) {
            uow.commit();
            out.applied = true;
            return out;
        }
    } catch (const std::invalid_argument& e) {
        // The service refused the proposed value, and a retry meets the same
        // answer.
        out.refusal = e.what();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Applying request " << request_id << " failed: " << e.what();
        return {.transient = true, .refusal = e.what()};
    }

    // The transaction is rolled back, so nothing was written. The request ends
    // here: a refusal does not change on a retry.
    out.groups_applied = 0;
    try {
        const auto moved = lifecycle.fail_apply(
            request_id, "The proposal could not be applied: " + out.refusal);
        if (moved.outcome != "ok") {
            BOOST_LOG_SEV(lg(), warn) << "Request " << request_id << " was refused but is "
                                      << moved.state_code << ", not apply_failed: "
                                      << moved.message;
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Request " << request_id
                                   << " could not be moved to apply_failed: " << e.what();
        return {.transient = true, .refusal = e.what()};
    }
    return out;
}

}
