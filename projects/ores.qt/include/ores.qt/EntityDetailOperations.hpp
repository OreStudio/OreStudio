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
#ifndef ORES_QT_ENTITY_DETAIL_OPERATIONS_HPP
#define ORES_QT_ENTITY_DETAIL_OPERATIONS_HPP

#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>

namespace ores::qt {

class ClientManager;

/**
 * @brief Common result type for save and delete operations.
 */
struct operation_result final {
    bool success;
    std::string message;
};

/**
 * @brief Lightweight struct for populating parent entity combo boxes.
 */
struct parent_entity_entry final {
    boost::uuids::uuid id;
    std::string short_code;
    std::string full_name;
    std::string status;
    std::optional<boost::uuids::uuid> parent_id;
};

/**
 * @brief Lightweight struct for populating identifier tables.
 */
struct identifier_entry final {
    boost::uuids::uuid id;
    boost::uuids::uuid owner_id; ///< party_id or counterparty_id
    std::string id_scheme;
    std::string id_value;
    std::string description;
    std::string modified_by;
    std::string performed_by;
};

/**
 * @brief Lightweight struct for populating contact tables.
 */
struct contact_entry final {
    boost::uuids::uuid id;
    boost::uuids::uuid owner_id; ///< party_id or counterparty_id
    std::string contact_type;
    std::string street_line_1;
    std::string street_line_2;
    std::string city;
    std::string state;
    std::string country_code;
    std::string postal_code;
    std::string phone;
    std::string email;
    std::string web_page;
    std::string modified_by;
    std::string performed_by;
};

/**
 * @brief Common entity data extracted from party or counterparty types.
 *
 * Provides a uniform view over the shared fields of party and counterparty
 * domain types.
 */
struct entity_data final {
    int version = 0;
    std::string tenant_id;
    boost::uuids::uuid id;
    std::string full_name;
    std::string short_code;
    std::optional<std::string> transliterated_name;
    std::optional<std::string> party_category; ///< Party-only field.
    std::string party_type;
    std::optional<boost::uuids::uuid> parent_id;
    std::string business_center_code;
    std::string status;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Result of loading all entities for hierarchy/parent combo.
 */
struct load_all_entities_result final {
    std::vector<parent_entity_entry> entities;
    bool success;
};

/**
 * @brief Result of loading identifiers for an entity.
 */
struct load_identifiers_result final {
    std::vector<identifier_entry> identifiers;
    bool success;
};

/**
 * @brief Result of loading contacts for an entity.
 */
struct load_contacts_result final {
    std::vector<contact_entry> contacts;
    bool success;
};

/**
 * @brief Interface for entity-specific operations in the detail dialog.
 *
 * Abstracts the differences between party and counterparty protocol
 * messages, allowing the EntityDetailDialog to share all UI code.
 * Implementations are synchronous and intended to be called from
 * QtConcurrent::run.
 */
class entity_detail_operations {
public:
    virtual ~entity_detail_operations() = default;

    /// Display name for the entity type (e.g. "Party", "Counterparty").
    [[nodiscard]] virtual std::string entity_type_name() const = 0;

    /// Whether this entity type has a party_category field.
    [[nodiscard]] virtual bool has_party_category() const = 0;

    /// Save the entity via protocol messages.
    [[nodiscard]] virtual operation_result save_entity(
        ClientManager* cm, const entity_data& data) const = 0;

    /// Delete the entity via protocol messages.
    [[nodiscard]] virtual operation_result delete_entity(
        ClientManager* cm, const boost::uuids::uuid& id) const = 0;

    /// Load all entities for parent combo and hierarchy tree.
    [[nodiscard]] virtual load_all_entities_result load_all_entities(
        ClientManager* cm) const = 0;

    /// Load identifiers for a specific entity.
    [[nodiscard]] virtual load_identifiers_result load_identifiers(
        ClientManager* cm, const boost::uuids::uuid& entity_id) const = 0;

    /// Save an identifier.
    [[nodiscard]] virtual operation_result save_identifier(
        ClientManager* cm, const identifier_entry& entry) const = 0;

    /// Delete an identifier.
    [[nodiscard]] virtual operation_result delete_identifier(
        ClientManager* cm, const boost::uuids::uuid& id) const = 0;

    /// Load contacts for a specific entity.
    [[nodiscard]] virtual load_contacts_result load_contacts(
        ClientManager* cm, const boost::uuids::uuid& entity_id) const = 0;

    /// Save a contact.
    [[nodiscard]] virtual operation_result save_contact(
        ClientManager* cm, const contact_entry& entry) const = 0;

    /// Delete a contact.
    [[nodiscard]] virtual operation_result delete_contact(
        ClientManager* cm, const boost::uuids::uuid& id) const = 0;
};

}

// Forward declarations for domain types used by conversion functions.
namespace ores::refdata::domain {
struct counterparty;
struct party;
}

namespace ores::qt {

/**
 * @brief Convert a counterparty domain object to entity_data.
 */
entity_data to_entity_data(const refdata::domain::counterparty& cpty);

/**
 * @brief Convert a party domain object to entity_data.
 */
entity_data to_entity_data(const refdata::domain::party& party);

}

#endif
