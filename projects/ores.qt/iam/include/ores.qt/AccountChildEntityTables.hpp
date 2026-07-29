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
#ifndef ORES_QT_ACCOUNT_CHILD_ENTITY_TABLES_HPP
#define ORES_QT_ACCOUNT_CHILD_ENTITY_TABLES_HPP

#include "ores.iam.api/domain/account_contact_information.hpp"
#include <QObject>
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

class QTabWidget;
class QWidget;

namespace ores::qt {

class ChildEntityTableWidget;
class ClientManager;
class ImageCache;
class ChangeReasonCache;

/**
 * @brief Owns and drives the "Contact Information" tab embedded in
 * AccountDetailDialog, mirroring PartyChildEntityTables (see that class for
 * the full rationale) but scoped to account's one composite child --
 * accounts have no identifiers entity. In practice an account has at most
 * one contact-information row (accounts_email_uniq_idx-style unique index
 * on account_id), but the table+popup-dialog pattern still works fine at
 * that cardinality and stays consistent with party/counterparty.
 */
class AccountChildEntityTables final : public QObject {
    Q_OBJECT

public:
    explicit AccountChildEntityTables(QWidget* dialogParent);

    /** @brief Add the tab to the dialog's tab widget. Call once, from the constructor. */
    void attachTo(QTabWidget* tabWidget);

    /** @brief Re-fetch and repopulate the table for the given account. No-op if accountId is nil.
     */
    void reload(const boost::uuids::uuid& accountId,
                ClientManager* clientManager,
                const std::string& username,
                ImageCache* imageCache = nullptr,
                ChangeReasonCache* changeReasonCache = nullptr);

    /** @brief Disable add/delete/edit while the parent dialog is read-only (e.g. viewing history).
     */
    void setReadOnly(bool readOnly);

private:
    void loadContacts();
    void onAddContact();
    void onDeleteContact(int row);
    void onEditContact(int row);

    QWidget* dialogParent_;
    ChildEntityTableWidget* contactTable_{nullptr};
    boost::uuids::uuid accountId_;
    ClientManager* clientManager_{nullptr};
    ImageCache* imageCache_{nullptr};
    ChangeReasonCache* changeReasonCache_{nullptr};
    std::string username_;
    bool readOnly_{false};
    std::vector<ores::iam::domain::account_contact_information> contacts_;
};

}

#endif
