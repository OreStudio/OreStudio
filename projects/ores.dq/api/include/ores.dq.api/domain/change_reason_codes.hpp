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
#ifndef ORES_DQ_API_DOMAIN_CHANGE_REASON_CODES_HPP
#define ORES_DQ_API_DOMAIN_CHANGE_REASON_CODES_HPP

namespace ores::dq::domain {

/**
 * @brief Well-known reason codes used throughout the system.
 *
 * These must match the codes seeded in ores_dq_change_reasons_tbl. Kept in
 * its own hand-maintained header, separate from the generated
 * change_reason.hpp, since codegen has no way to express "also emit this
 * extra namespace of constants" -- a regeneration of change_reason.hpp
 * would otherwise silently drop it.
 */
namespace change_reasons {
// System reasons
constexpr auto system_initial_load = "system.initial_load";
constexpr auto system_new_record = "system.new_record";
constexpr auto system_external_data_import = "system.external_data_import";
constexpr auto system_import = "system.import";
constexpr auto system_test = "system.test";
constexpr auto system_tenant_terminated = "system.tenant_terminated";

// Static data reasons
constexpr auto static_data_front_office_error = "static_data.front_office_error";
constexpr auto static_data_back_office_error = "static_data.back_office_error";
constexpr auto static_data_regulatory_change = "static_data.regulatory_change";
constexpr auto static_data_corporate_action = "static_data.corporate_action";
}

}

#endif
