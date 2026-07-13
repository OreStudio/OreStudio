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
#ifndef ORES_QT_PARTY_CHILD_ENTITY_TABLES_HPP
#define ORES_QT_PARTY_CHILD_ENTITY_TABLES_HPP

#include "ores.refdata.api/domain/party_contact_information.hpp"
#include "ores.refdata.api/domain/party_identifier.hpp"
#include <QObject>
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

class QTabWidget;
class QWidget;

namespace ores::qt {

class ChildEntityTableWidget;
class ClientManager;

/**
 * @brief Owns and drives the "Identifiers" and "Contact Information" tabs
 * embedded in PartyDetailDialog -- party's two composite children under
 * temporal versioning (see the "Temporal composite entity versioning"
 * architecture doc). Not codegen'd: hand-written once here and wired into
 * the generated dialog via a small paste block, rather than duplicated
 * inline in the entity org model.
 *
 * Row add/delete uses a fixed change-reason code rather than prompting via
 * ChangeReasonDialog, matching the pre-codegen dialog's behaviour.
 */
class PartyChildEntityTables final : public QObject {
    Q_OBJECT

public:
    explicit PartyChildEntityTables(QWidget* dialogParent);

    /** @brief Add both tabs to the dialog's tab widget. Call once, from the constructor. */
    void attachTo(QTabWidget* tabWidget);

    /** @brief Re-fetch and repopulate both tables for the given party. No-op if partyId is nil. */
    void reload(const boost::uuids::uuid& partyId,
                ClientManager* clientManager,
                const std::string& username);

    /** @brief Disable add/delete/edit while the parent dialog is read-only (e.g. viewing history). */
    void setReadOnly(bool readOnly);

private:
    void loadIdentifiers();
    void loadContacts();
    void onAddIdentifier();
    void onDeleteIdentifier(int row);
    void onAddContact();
    void onDeleteContact(int row);
    void onEditContact(int row);

    QWidget* dialogParent_;
    ChildEntityTableWidget* identifierTable_{nullptr};
    ChildEntityTableWidget* contactTable_{nullptr};
    boost::uuids::uuid partyId_;
    ClientManager* clientManager_{nullptr};
    std::string username_;
    bool readOnly_{false};
    std::vector<ores::refdata::domain::party_identifier> identifiers_;
    std::vector<ores::refdata::domain::party_contact_information> contacts_;
};

}

#endif
