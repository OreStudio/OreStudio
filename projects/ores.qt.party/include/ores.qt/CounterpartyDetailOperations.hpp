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
#ifndef ORES_QT_COUNTERPARTY_DETAIL_OPERATIONS_HPP
#define ORES_QT_COUNTERPARTY_DETAIL_OPERATIONS_HPP

#include "ores.qt/EntityDetailOperations.hpp"

namespace ores::qt {

/**
 * @brief Counterparty-specific operations for the entity detail dialog.
 *
 * Implements entity_detail_operations using counterparty protocol messages.
 */
class counterparty_detail_operations final : public entity_detail_operations {
public:
    [[nodiscard]] std::string entity_type_name() const override;
    [[nodiscard]] bool has_party_category() const override;

    [[nodiscard]] operation_result save_entity(
        ClientManager* cm, const entity_data& data) const override;
    [[nodiscard]] operation_result delete_entity(
        ClientManager* cm, const boost::uuids::uuid& id) const override;
    [[nodiscard]] load_all_entities_result load_all_entities(
        ClientManager* cm) const override;

    [[nodiscard]] load_identifiers_result load_identifiers(
        ClientManager* cm, const boost::uuids::uuid& entity_id) const override;
    [[nodiscard]] operation_result save_identifier(
        ClientManager* cm, const identifier_entry& entry) const override;
    [[nodiscard]] operation_result delete_identifier(
        ClientManager* cm, const boost::uuids::uuid& id) const override;

    [[nodiscard]] load_contacts_result load_contacts(
        ClientManager* cm, const boost::uuids::uuid& entity_id) const override;
    [[nodiscard]] operation_result save_contact(
        ClientManager* cm, const contact_entry& entry) const override;
    [[nodiscard]] operation_result delete_contact(
        ClientManager* cm, const boost::uuids::uuid& id) const override;
};

}

#endif
