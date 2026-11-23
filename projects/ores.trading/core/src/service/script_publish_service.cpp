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
#include "ores.trading.core/service/script_publish_service.hpp"
#include "script_diagnostics.hpp"
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.trading.core/repository/script_calibration_repository.hpp"
#include "ores.trading.core/repository/script_calibration_strike_repository.hpp"
#include "ores.trading.core/repository/script_coarsening_repository.hpp"
#include "ores.trading.core/repository/script_input_repository.hpp"
#include "ores.trading.core/repository/script_repository.hpp"
#include "ores.trading.core/repository/script_result_repository.hpp"
#include "ores.trading.core/repository/script_variant_repository.hpp"
#include "ores.trading.scripting/inputs.hpp"
#include "ores.trading.scripting/parse.hpp"
#include <algorithm>
#include <format>
#include <optional>
#include <set>
#include <string>
#include <tuple>
#include <vector>

namespace ores::trading::service {

namespace {

using messaging::publish_script_request;
using messaging::publish_script_response;

constexpr std::string_view publish_default_purpose = "default";
constexpr std::string_view publish_commentary = "Published by the tenant";
constexpr std::string_view publish_writer = "ores.trading.service";

/// The rows of one version of a script, as the store holds them.
struct published_script_rows final {
    std::optional<domain::script> script;
    std::vector<domain::script_variant> variants;
    std::vector<domain::script_result> results;
    std::vector<domain::script_calibration> calibrations;
    std::vector<domain::script_calibration_strike> strikes;
    std::vector<domain::script_coarsening> coarsenings;
    std::vector<domain::script_input> inputs;
};

/// A field of a row's content, prefixed by its length, so that no two rows of
/// different fields give the same content, and an absent value differs from an empty one.
std::string published_field(std::string_view value) {
    return std::format("{}:{}", value.size(), value);
}

std::string published_field(const std::string& value) {
    return published_field(std::string_view(value));
}

std::string published_field(const std::optional<std::string>& value) {
    return value ? published_field(*value) : std::string("~");
}

std::string published_field(int value) {
    return published_field(std::to_string(value));
}

std::string published_field(bool value) {
    return published_field(std::string_view(value ? "true" : "false"));
}

/// One row's content: its table and its fields, each length-prefixed.
template <typename... Fields>
std::string published_row(std::string_view table, const Fields&... fields) {
    std::string out(table);
    ((out += published_field(fields)), ...);
    return out;
}

/// What each row of a version states, without its version or audit, in key order.
std::vector<std::string> published_content_of(const published_script_rows& rows) {
    std::vector<std::string> out;
    if (rows.script)
        out.push_back(published_row("script", rows.script->product_tag, rows.script->description));
    for (const auto& v : rows.variants)
        out.push_back(published_row("variant", v.purpose, v.code, v.npv_variable));
    for (const auto& r : rows.results)
        out.push_back(published_row("result", r.purpose, r.sequence_number, r.variable, r.rename));
    for (const auto& c : rows.calibrations)
        out.push_back(published_row("calibration", c.purpose, c.sequence_number, c.index_variable));
    for (const auto& k : rows.strikes)
        out.push_back(published_row(
            "strike", k.purpose, k.calibration_number, k.sequence_number, k.strike_variable));
    for (const auto& o : rows.coarsenings)
        out.push_back(
            published_row("coarsening", o.purpose, o.sequence_number, o.eligible_schedule));
    for (const auto& i : rows.inputs)
        out.push_back(published_row(
            "input", i.sequence_number, i.input_name, i.data_type, i.is_array, i.value_rule));
    std::ranges::sort(out);
    return out;
}

/// The current rows of the tenant's own script, leaving out the world library's.
published_script_rows published_current_rows(const ores::database::context& ctx,
                                             const std::string& name) {
    const auto own = [&](const auto& row) {
        return row.tenant_id == ctx.tenant_id();
    };
    const auto of_script = [&](const auto& row) {
        return own(row) && row.script_name == name;
    };
    const auto keep = [](auto rows, const auto& predicate) {
        std::erase_if(rows, [&](const auto& row) { return !predicate(row); });
        return rows;
    };

    published_script_rows out;
    for (auto& s : repository::script_repository().read_latest(ctx, name))
        if (own(s))
            out.script = std::move(s);
    out.variants = keep(repository::script_variant_repository().read_latest(ctx), of_script);
    out.results = keep(repository::script_result_repository().read_latest(ctx), of_script);
    out.calibrations =
        keep(repository::script_calibration_repository().read_latest(ctx), of_script);
    out.strikes =
        keep(repository::script_calibration_strike_repository().read_latest(ctx), of_script);
    out.coarsenings = keep(repository::script_coarsening_repository().read_latest(ctx), of_script);
    out.inputs = keep(repository::script_input_repository().read_latest(ctx), of_script);
    return out;
}

/// The rows a request states, with the inputs its variants derive and no version yet.
published_script_rows published_requested_rows(const publish_script_request& request,
                                               const std::vector<scripting::script_input>& inputs) {
    published_script_rows out;
    domain::script script;
    script.name = request.name;
    script.product_tag = request.product_tag;
    script.description = request.description;
    out.script = std::move(script);

    for (const auto& v : request.variants) {
        domain::script_variant variant;
        variant.script_name = request.name;
        variant.purpose = v.purpose;
        variant.code = v.code;
        variant.npv_variable = v.npv_variable;
        out.variants.push_back(std::move(variant));

        for (std::size_t i = 0; i < v.results.size(); ++i) {
            domain::script_result result;
            result.script_name = request.name;
            result.purpose = v.purpose;
            result.sequence_number = static_cast<int>(i + 1);
            result.variable = v.results[i].variable;
            result.rename = v.results[i].rename;
            out.results.push_back(std::move(result));
        }
        for (std::size_t i = 0; i < v.calibrations.size(); ++i) {
            domain::script_calibration calibration;
            calibration.script_name = request.name;
            calibration.purpose = v.purpose;
            calibration.sequence_number = static_cast<int>(i + 1);
            calibration.index_variable = v.calibrations[i].index_variable;
            out.calibrations.push_back(std::move(calibration));
            for (std::size_t j = 0; j < v.calibrations[i].strikes.size(); ++j) {
                domain::script_calibration_strike strike;
                strike.script_name = request.name;
                strike.purpose = v.purpose;
                strike.calibration_number = static_cast<int>(i + 1);
                strike.sequence_number = static_cast<int>(j + 1);
                strike.strike_variable = v.calibrations[i].strikes[j];
                out.strikes.push_back(std::move(strike));
            }
        }
        for (std::size_t i = 0; i < v.coarsenings.size(); ++i) {
            domain::script_coarsening coarsening;
            coarsening.script_name = request.name;
            coarsening.purpose = v.purpose;
            coarsening.sequence_number = static_cast<int>(i + 1);
            coarsening.eligible_schedule = v.coarsenings[i];
            out.coarsenings.push_back(std::move(coarsening));
        }
    }

    for (std::size_t i = 0; i < inputs.size(); ++i) {
        domain::script_input input;
        input.script_name = request.name;
        input.sequence_number = static_cast<int>(i + 1);
        input.input_name = inputs[i].name;
        input.data_type = inputs[i].type;
        input.is_array = inputs[i].is_array;
        input.value_rule = inputs[i].rule;
        out.inputs.push_back(std::move(input));
    }
    return out;
}

/// Stamps a row as the caller's, at the version the script was given.
template <typename Row>
void stamp_published_row(Row& row,
                         const ores::database::context& ctx,
                         std::string_view reason,
                         int version = 0) {
    // The session's actor and service replace these; a context with neither,
    // as a test's, still writes a row the store accepts.
    row.modified_by = row.performed_by = std::string(publish_writer);
    row.change_reason_code.clear();
    ores::service::messaging::stamp(row, ctx, reason);
    row.change_commentary = std::string(publish_commentary);
    if constexpr (requires { row.script_version; })
        row.script_version = version;
}

/// Closes each current row whose key the new version does not hold.
template <typename Row, typename Key, typename Remove>
void close_dropped_published_rows(const std::vector<Row>& current,
                                  const std::vector<Row>& wanted,
                                  const Key& key_of,
                                  const Remove& remove) {
    // decltype does not evaluate its operand, so an empty current is safe here.
    std::set<decltype(key_of(current.front()))> kept;
    for (const auto& row : wanted)
        kept.insert(key_of(row));
    for (const auto& row : current)
        if (!kept.contains(key_of(row)))
            remove(row);
}

publish_script_response publish_refused(std::string message) {
    publish_script_response response;
    response.message = std::move(message);
    return response;
}

}

script_publish_service::script_publish_service(const ores::database::context& ctx)
    : ctx_(ctx) {}

publish_script_response script_publish_service::publish(const publish_script_request& request) {
    if (ctx_.tenant_id().is_system())
        return publish_refused("The world library is published from its dataset, not by a tenant.");
    if (request.name.empty())
        return publish_refused("A published script states its name.");
    if (request.variants.empty())
        return publish_refused("A published script states at least one variant.");

    std::set<std::string> purposes;
    for (const auto& v : request.variants) {
        if (v.purpose.empty())
            return publish_refused(
                "Each variant states its purpose; the one trades price with is " +
                std::string(publish_default_purpose) + ".");
        if (!purposes.insert(v.purpose).second)
            return publish_refused("The purpose " + v.purpose +
                                   " is stated by more than one variant.");
    }
    for (const auto& v : request.variants) {
        if (v.code.empty() || v.npv_variable.empty())
            return publish_refused("The variant " + v.purpose +
                                   " states its code and the variable its NPV is in.");
        const auto unnamed = [](const auto& names) {
            return std::ranges::any_of(names, [](const std::string& n) { return n.empty(); });
        };
        std::vector<std::string> named;
        for (const auto& r : v.results)
            named.push_back(r.variable);
        for (const auto& c : v.calibrations) {
            named.push_back(c.index_variable);
            named.insert(named.end(), c.strikes.begin(), c.strikes.end());
        }
        named.insert(named.end(), v.coarsenings.begin(), v.coarsenings.end());
        if (unnamed(named))
            return publish_refused("The variant " + v.purpose +
                                   " names a result, calibration or schedule with no variable.");
    }
    if (!purposes.contains(std::string(publish_default_purpose)))
        return publish_refused("A published script has a " + std::string(publish_default_purpose) +
                               " variant, which its trades price with.");

    // The default variant's inputs come first, as a trade states those it
    // prices with before those of any other purpose.
    auto ordered = request.variants;
    std::ranges::stable_partition(
        ordered, [](const auto& v) { return v.purpose == publish_default_purpose; });

    publish_script_response response;
    std::vector<std::vector<scripting::script_input>> inputs_by_variant;
    auto errors = 0;
    for (const auto& v : ordered) {
        const auto parsed = scripting::parse(v.code);
        for (const auto& d : parsed.diagnostics) {
            if (d.severity == scripting::severity::error)
                ++errors;
            response.diagnostics.push_back(to_wire(d, v.purpose));
        }
        if (parsed.parsed())
            inputs_by_variant.push_back(scripting::inputs_of(parsed.tree, v.code));
    }
    if (errors > 0) {
        response.message = std::format(
            "The script {} is refused: its parse reports {} error(s).", request.name, errors);
        return response;
    }

    const auto inputs = scripting::merge_inputs(inputs_by_variant);
    auto wanted = published_requested_rows(request, inputs);


    ores::database::repository::unit_of_work uow(ctx_);
    const auto& ctx = uow.ctx();

    repository::script_repository scripts;
    const auto world_holds_it =
        std::ranges::any_of(scripts.read_latest(ctx, request.name),
                            [](const auto& s) { return s.tenant_id.is_system(); });
    if (world_holds_it)
        response.warnings.push_back(std::format(
            "The world library also holds a script named {}. The tenant's trades that name it "
            "now book on the tenant's own script.",
            request.name));

    const auto current = published_current_rows(ctx, request.name);
    if (current.script && published_content_of(current) == published_content_of(wanted)) {
        response.success = true;
        response.version = current.script->version;
        response.message =
            std::format("The script {} equals its version {}, so no version is written.",
                        request.name,
                        response.version);
        return response;
    }

    const auto reason = current.script ? ores::service::messaging::change_reasons::update :
                                         ores::service::messaging::change_reasons::new_record;
    stamp_published_row(*wanted.script, ctx, reason);
    scripts.write(ctx, *wanted.script);

    int version = 0;
    for (const auto& s : scripts.read_latest(ctx, request.name))
        if (s.tenant_id == ctx.tenant_id())
            version = s.version;

    const auto& name = request.name;
    close_dropped_published_rows(
        current.variants,
        wanted.variants,
        [](const auto& r) { return r.purpose; },
        [&](const auto& r) {
            repository::script_variant_repository().remove(ctx, name, r.purpose);
        });
    close_dropped_published_rows(
        current.results,
        wanted.results,
        [](const auto& r) { return std::make_tuple(r.purpose, r.sequence_number); },
        [&](const auto& r) {
            repository::script_result_repository().remove(
                ctx, name, r.purpose, std::to_string(r.sequence_number));
        });
    close_dropped_published_rows(
        current.calibrations,
        wanted.calibrations,
        [](const auto& r) { return std::make_tuple(r.purpose, r.sequence_number); },
        [&](const auto& r) {
            repository::script_calibration_repository().remove(
                ctx, name, r.purpose, std::to_string(r.sequence_number));
        });
    close_dropped_published_rows(
        current.strikes,
        wanted.strikes,
        [](const auto& r) {
            return std::make_tuple(r.purpose, r.calibration_number, r.sequence_number);
        },
        [&](const auto& r) {
            repository::script_calibration_strike_repository().remove(
                ctx,
                name,
                r.purpose,
                std::to_string(r.calibration_number),
                std::to_string(r.sequence_number));
        });
    close_dropped_published_rows(
        current.coarsenings,
        wanted.coarsenings,
        [](const auto& r) { return std::make_tuple(r.purpose, r.sequence_number); },
        [&](const auto& r) {
            repository::script_coarsening_repository().remove(
                ctx, name, r.purpose, std::to_string(r.sequence_number));
        });
    close_dropped_published_rows(
        current.inputs,
        wanted.inputs,
        [](const auto& r) { return r.sequence_number; },
        [&](const auto& r) {
            repository::script_input_repository().remove(
                ctx, name, std::to_string(r.sequence_number));
        });

    const auto write_all = [&](auto& rows, auto repo) {
        if (rows.empty())
            return;
        for (auto& row : rows)
            stamp_published_row(row, ctx, reason, version);
        repo.write(ctx, rows);
    };
    write_all(wanted.variants, repository::script_variant_repository());
    write_all(wanted.results, repository::script_result_repository());
    write_all(wanted.calibrations, repository::script_calibration_repository());
    write_all(wanted.strikes, repository::script_calibration_strike_repository());
    write_all(wanted.coarsenings, repository::script_coarsening_repository());
    write_all(wanted.inputs, repository::script_input_repository());

    uow.commit();

    response.success = true;
    response.version = version;
    response.message = std::format("The script {} is published as version {}.", name, version);
    ORES_LOG_SEV(lg(), info) << response.message << " Tenant " << ctx_.tenant_id().to_string();
    return response;
}

}
