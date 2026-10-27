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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_generator.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.analytics.api/generators/sensitivity_asset_class_generator.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <string>
#include <unordered_set>

namespace ores::analytics::generators {

using ores::utility::generation::generation_keys;

domain::sensitivity_asset_class
generate_synthetic_sensitivity_asset_class(utility::generation::generation_context& ctx) {
    [[maybe_unused]] static std::atomic<int> counter{0};
    const auto modified_by = ctx.env().get_or(std::string(generation_keys::modified_by), "system");
    const auto tid_str =
        ctx.env().get_or(std::string(generation_keys::tenant_id), std::string("system"));

    domain::sensitivity_asset_class r;
    r.version = 0;
    r.tenant_id =
        utility::uuid::tenant_id::from_string(tid_str).value_or(utility::uuid::tenant_id::system());
    const auto idx = counter.fetch_add(1, std::memory_order_relaxed);
    r.asset_class_code = [idx] {
        static constexpr std::string_view codes[] = {
            "interest_rates", "fx", "equity", "commodity", "credit"};
        return std::string(codes[static_cast<std::size_t>(idx) % std::size(codes)]);
    }() + "-" + std::to_string(idx);
    r.template_prefix = [&r] {
        static constexpr std::string_view codes[] = {
            "interest_rates", "fx", "equity", "commodity", "credit"};
        static constexpr std::string_view prefixes[] = {"IR", "FX", "EQ", "COMM", "CR"};
        for (std::size_t i = 0; i < std::size(codes); ++i)
            if (r.asset_class_code == codes[i])
                return std::string(prefixes[i]);
        return std::string("IR");
    }();
    r.name = [&r] {
        static constexpr std::string_view codes[] = {
            "interest_rates", "fx", "equity", "commodity", "credit"};
        static constexpr std::string_view names[] = {
            "Interest rate", "Foreign exchange", "Equity", "Commodity", "Credit"};
        for (std::size_t i = 0; i < std::size(codes); ++i)
            if (r.asset_class_code == codes[i])
                return std::string(names[i]);
        return std::string("Interest rate");
    }();
    r.description = std::string(faker::lorem::sentence());
    r.display_order = static_cast<int>(idx);
    r.modified_by = modified_by;
    r.performed_by = modified_by;
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<domain::sensitivity_asset_class>
generate_synthetic_sensitivity_asset_classes(std::size_t n,
                                             utility::generation::generation_context& ctx) {
    std::vector<domain::sensitivity_asset_class> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_sensitivity_asset_class(ctx));
    return r;
}

}
