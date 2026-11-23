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
#include "ores.trading.core/service/script_validation_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.trading.core/repository/script_library_version_repository.hpp"
#include "ores.trading.core/repository/script_repository.hpp"
#include "ores.trading.scripting/check.hpp"
#include "ores.trading.scripting/inputs.hpp"
#include "ores.trading.scripting/parse.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "script_diagnostics.hpp"
#include <format>
#include <set>
#include <tuple>

namespace ores::trading::service {

messaging::script_diagnostic to_wire(const scripting::diagnostic& d, const std::string& purpose) {
    messaging::script_diagnostic out;
    out.purpose = purpose;
    out.severity = d.severity == scripting::severity::error ? "error" : "warning";
    out.code = d.code;
    out.message = d.message;
    out.subject = d.subject.value_or("");
    if (d.start) {
        out.line = static_cast<int>(d.start->line);
        if (d.start->column)
            out.column = static_cast<int>(*d.start->column);
    }
    return out;
}

namespace {

std::vector<scripting::script_variable>
to_variables(const std::vector<messaging::script_variable_value>& values) {
    std::vector<scripting::script_variable> out;
    out.reserve(values.size());
    for (const auto& v : values)
        out.push_back({.name = v.name, .type = v.type, .is_array = v.is_array, .values = v.values});
    return out;
}

}

messaging::validate_script_response
validate_script(std::span<const script_text> variants,
                const std::vector<messaging::script_variable_value>& values) {
    const auto variables = to_variables(values);
    messaging::validate_script_response out;
    if (variants.empty()) {
        out.message = "There is no script to validate: it has no variant.";
        return out;
    }

    std::vector<scripting::parse_result> parsed;
    std::vector<std::vector<scripting::script_input>> inputs_by_variant;
    for (const auto& v : variants) {
        auto result = scripting::parse(v.code);
        for (const auto& d : result.diagnostics)
            out.diagnostics.push_back(to_wire(d, v.purpose));
        if (result.parsed())
            inputs_by_variant.push_back(scripting::inputs_of(result.tree, v.code));
        parsed.push_back(std::move(result));
    }

    if (inputs_by_variant.size() == variants.size()) {
        const auto inputs = scripting::merge_inputs(inputs_by_variant);
        std::set<std::tuple<std::string, std::string, std::string>> reported;
        for (std::size_t i = 0; i < variants.size(); ++i) {
            for (const auto& d :
                 scripting::check(parsed[i].tree, variants[i].code, inputs, variables)) {
                if (d.subject && !reported.emplace(d.code, *d.subject, d.message).second)
                    continue;
                out.diagnostics.push_back(
                    to_wire(d, d.subject ? std::string{} : variants[i].purpose));
            }
        }
    }

    auto errors = 0;
    auto warnings = 0;
    for (const auto& d : out.diagnostics)
        (d.severity == "error" ? errors : warnings) += 1;
    out.success = true;
    out.is_valid = errors == 0;
    out.message = std::format("{} error(s) and {} warning(s).", errors, warnings);
    return out;
}

std::optional<resolved_script>
resolve_library_script(const ores::database::context& ctx, const std::string& name, int version) {
    using ores::utility::uuid::tenant_id;
    if (version < 0)
        return std::nullopt;

    // The pins to try, in the order the caller's tenant resolves the name: its own
    // script first, then the world library's.
    std::vector<messaging::script_pin> pins;
    if (version == 0) {
        // The read returns the tenant's own row and the system tenant's under
        // row-level security; the tenant's own replaces a system one found first.
        const domain::script* chosen = nullptr;
        const auto scripts = repository::script_repository().read_latest(ctx, name);
        for (const auto& s : scripts)
            if (s.name == name && (chosen == nullptr || chosen->tenant_id.is_system()))
                chosen = &s;
        if (chosen == nullptr)
            return std::nullopt;
        pins.push_back(
            {.tenant_id = chosen->tenant_id.to_uuid(), .name = name, .version = chosen->version});
    } else {
        for (const auto& tenant : {ctx.tenant_id(), tenant_id::system()})
            pins.push_back({.tenant_id = tenant.to_uuid(), .name = name, .version = version});
    }

    for (const auto& pin : pins) {
        messaging::export_script_library_response held;
        repository::script_library_version_repository().read(ctx, {pin}, held);
        if (held.scripts.empty())
            continue;
        resolved_script out{.tenant_id = pin.tenant_id, .version = held.scripts.front().version};
        for (const auto& v : held.variants)
            if (v.script_name == name)
                out.variants.push_back({.purpose = v.purpose, .code = v.code});
        return out;
    }
    return std::nullopt;
}

std::optional<std::vector<script_text>>
library_variants(const ores::database::context& ctx, const std::string& name, int version) {
    auto script = resolve_library_script(ctx, name, version);
    if (!script)
        return std::nullopt;
    return std::move(script->variants);
}

}
