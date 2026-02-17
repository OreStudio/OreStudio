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
#include "ores.qt/EntityDetailOperations.hpp"
#include "ores.refdata/domain/counterparty.hpp"
#include "ores.refdata/domain/party.hpp"

namespace ores::qt {

entity_data to_entity_data(const refdata::domain::counterparty& cpty) {
    entity_data d;
    d.version = cpty.version;
    d.tenant_id = cpty.tenant_id;
    d.id = cpty.id;
    d.full_name = cpty.full_name;
    d.short_code = cpty.short_code;
    d.transliterated_name = cpty.transliterated_name;
    d.party_category = std::nullopt;
    d.party_type = cpty.party_type;
    d.parent_id = cpty.parent_counterparty_id;
    d.business_center_code = cpty.business_center_code;
    d.status = cpty.status;
    d.modified_by = cpty.modified_by;
    d.performed_by = cpty.performed_by;
    d.change_reason_code = cpty.change_reason_code;
    d.change_commentary = cpty.change_commentary;
    d.recorded_at = cpty.recorded_at;
    return d;
}

entity_data to_entity_data(const refdata::domain::party& p) {
    entity_data d;
    d.version = p.version;
    d.tenant_id = p.tenant_id;
    d.id = p.id;
    d.full_name = p.full_name;
    d.short_code = p.short_code;
    d.transliterated_name = p.transliterated_name;
    d.party_category = p.party_category;
    d.party_type = p.party_type;
    d.parent_id = p.parent_party_id;
    d.business_center_code = p.business_center_code;
    d.status = p.status;
    d.modified_by = p.modified_by;
    d.performed_by = p.performed_by;
    d.change_reason_code = p.change_reason_code;
    d.change_commentary = p.change_commentary;
    d.recorded_at = p.recorded_at;
    return d;
}

}
