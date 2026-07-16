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
#include "ores.qt/PartyChildEntityTables.hpp"
#include "ores.qt/ChildEntityTableWidget.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/PartyContactInformationDetailDialog.hpp"
#include "ores.qt/PartyIdentifierDetailDialog.hpp"
#include "ores.refdata.api/messaging/party_contact_information_protocol.hpp"
#include "ores.refdata.api/messaging/party_identifier_protocol.hpp"
#include <QDialog>
#include <QFutureWatcher>
#include <QPointer>
#include <QTabWidget>
#include <QTableWidget>
#include <QVBoxLayout>
#include <QtConcurrent/QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

PartyChildEntityTables::PartyChildEntityTables(QWidget* dialogParent)
    : QObject(dialogParent)
    , dialogParent_(dialogParent)
    , identifierTable_(new ChildEntityTableWidget(
          {"Scheme", "Value", "Description"}, "Add Identifier", "Delete Identifier", dialogParent))
    , contactTable_(new ChildEntityTableWidget({"Type", "Country", "Street", "City", "Phone"},
                                               "Add Contact",
                                               "Delete Contact",
                                               dialogParent)) {

    connect(identifierTable_,
            &ChildEntityTableWidget::addRequested,
            this,
            &PartyChildEntityTables::onAddIdentifier);
    connect(identifierTable_,
            &ChildEntityTableWidget::deleteRequested,
            this,
            &PartyChildEntityTables::onDeleteIdentifier);
    connect(contactTable_,
            &ChildEntityTableWidget::addRequested,
            this,
            &PartyChildEntityTables::onAddContact);
    connect(contactTable_,
            &ChildEntityTableWidget::deleteRequested,
            this,
            &PartyChildEntityTables::onDeleteContact);
    connect(contactTable_->table(),
            &QTableWidget::cellDoubleClicked,
            this,
            [this](int row, int /* column */) { onEditContact(row); });
    connect(identifierTable_->table(),
            &QTableWidget::cellDoubleClicked,
            this,
            [this](int row, int /* column */) { onEditIdentifier(row); });
}

void PartyChildEntityTables::attachTo(QTabWidget* tabWidget) {
    // Insert before the last (static, .ui-defined) tab -- Provenance --
    // so dynamically-attached tabs never push it out of the last slot.
    const int insertIndex = std::max(0, tabWidget->count() - 1);
    tabWidget->insertTab(insertIndex, identifierTable_, "Identifiers");
    tabWidget->insertTab(insertIndex + 1, contactTable_, "Contact Information");
}

void PartyChildEntityTables::reload(const boost::uuids::uuid& partyId,
                                    ClientManager* clientManager,
                                    const std::string& username,
                                    ImageCache* imageCache,
                                    ChangeReasonCache* changeReasonCache) {
    partyId_ = partyId;
    clientManager_ = clientManager;
    username_ = username;
    imageCache_ = imageCache;
    changeReasonCache_ = changeReasonCache;
    if (partyId_.is_nil() || !clientManager_ || !clientManager_->isConnected())
        return;
    loadIdentifiers();
    loadContacts();
}

void PartyChildEntityTables::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
}

void PartyChildEntityTables::loadIdentifiers() {
    QPointer<PartyChildEntityTables> self = this;
    auto* cm = clientManager_;
    const auto partyIdStr = boost::uuids::to_string(partyId_);

    auto task = [cm, partyIdStr]() -> std::vector<refdata::domain::party_identifier> {
        refdata::messaging::get_party_identifiers_by_party_id_request req;
        req.party_id = partyIdStr;
        req.limit = 1000;
        auto result = cm->process_authenticated_request(std::move(req));
        if (!result || !result->success)
            return {};
        return result->party_identifiers;
    };

    auto* watcher = new QFutureWatcher<std::vector<refdata::domain::party_identifier>>(this);
    connect(watcher,
            &QFutureWatcher<std::vector<refdata::domain::party_identifier>>::finished,
            this,
            [self, watcher]() {
                auto result = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                self->identifiers_ = std::move(result);
                std::vector<QStringList> rows;
                rows.reserve(self->identifiers_.size());
                for (const auto& ident : self->identifiers_) {
                    rows.push_back({QString::fromStdString(ident.id_scheme),
                                    QString::fromStdString(ident.id_value),
                                    QString::fromStdString(ident.description)});
                }
                self->identifierTable_->setRows(rows);
            });
    watcher->setFuture(QtConcurrent::run(task));
}

void PartyChildEntityTables::onAddIdentifier() {
    if (readOnly_)
        return;
    if (partyId_.is_nil()) {
        MessageBoxHelper::warning(
            dialogParent_, "Save Required", "Save the party first, then add identifiers.");
        return;
    }
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            dialogParent_, "Disconnected", "Cannot add identifier while disconnected from server.");
        return;
    }

    QDialog wrapper(dialogParent_);
    wrapper.setWindowTitle("Add Identifier");
    auto* layout = new QVBoxLayout(&wrapper);
    layout->setContentsMargins(0, 0, 0, 0);

    auto* dialog = new PartyIdentifierDetailDialog(&wrapper);
    layout->addWidget(dialog);
    dialog->setClientManager(clientManager_);
    dialog->setUsername(username_);
    dialog->setChangeReasonCache(changeReasonCache_);
    dialog->setImageCache(imageCache_);
    dialog->setCreateMode(true);

    refdata::domain::party_identifier newIdent;
    boost::uuids::random_generator uuid_gen;
    newIdent.id = uuid_gen();
    newIdent.party_id = partyId_;
    dialog->setIdentifier(newIdent);

    connect(dialog, &PartyIdentifierDetailDialog::partyIdentifierSaved, &wrapper, &QDialog::accept);

    if (wrapper.exec() == QDialog::Accepted)
        loadIdentifiers();
}

void PartyChildEntityTables::onDeleteIdentifier(int row) {
    if (!clientManager_ || !clientManager_->isConnected() || readOnly_ || row < 0 ||
        row >= static_cast<int>(identifiers_.size()))
        return;

    const auto id = identifiers_[static_cast<std::size_t>(row)].id;
    QPointer<PartyChildEntityTables> self = this;
    auto* cm = clientManager_;
    auto task = [cm, id]() -> std::pair<bool, QString> {
        refdata::messaging::delete_party_identifier_request req;
        req.ids.push_back(boost::uuids::to_string(id));
        auto result = cm->process_authenticated_request(std::move(req));
        if (!result)
            return {false, QString::fromStdString(result.error())};
        return {result->success, QString::fromStdString(result->message)};
    };
    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, this, [self, watcher]() {
        const auto [ok, message] = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        if (ok)
            self->loadIdentifiers();
        else
            MessageBoxHelper::warning(self->dialogParent_, "Delete Failed", message);
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void PartyChildEntityTables::onEditIdentifier(int row) {
    if (row < 0 || row >= static_cast<int>(identifiers_.size()))
        return;
    const auto ident = identifiers_[static_cast<std::size_t>(row)];

    QDialog wrapper(dialogParent_);
    wrapper.setWindowTitle("Identifier Details");
    auto* layout = new QVBoxLayout(&wrapper);
    layout->setContentsMargins(0, 0, 0, 0);

    auto* dialog = new PartyIdentifierDetailDialog(&wrapper);
    layout->addWidget(dialog);
    dialog->setClientManager(clientManager_);
    dialog->setUsername(username_);
    dialog->setChangeReasonCache(changeReasonCache_);
    dialog->setImageCache(imageCache_);
    dialog->setCreateMode(false);
    dialog->setReadOnly(readOnly_ || !clientManager_ || !clientManager_->isConnected());
    dialog->setIdentifier(ident);

    connect(dialog, &PartyIdentifierDetailDialog::partyIdentifierSaved, &wrapper, &QDialog::accept);

    if (wrapper.exec() == QDialog::Accepted)
        loadIdentifiers();
}

void PartyChildEntityTables::loadContacts() {
    QPointer<PartyChildEntityTables> self = this;
    auto* cm = clientManager_;
    const auto partyIdStr = boost::uuids::to_string(partyId_);

    auto task = [cm, partyIdStr]() -> std::vector<refdata::domain::party_contact_information> {
        refdata::messaging::get_party_contact_informations_by_party_id_request req;
        req.party_id = partyIdStr;
        req.limit = 1000;
        auto result = cm->process_authenticated_request(std::move(req));
        if (!result || !result->success)
            return {};
        return result->party_contact_informations;
    };

    auto* watcher =
        new QFutureWatcher<std::vector<refdata::domain::party_contact_information>>(this);
    connect(watcher,
            &QFutureWatcher<std::vector<refdata::domain::party_contact_information>>::finished,
            this,
            [self, watcher]() {
                auto result = watcher->result();
                watcher->deleteLater();
                if (!self)
                    return;
                self->contacts_ = std::move(result);
                std::vector<QStringList> rows;
                rows.reserve(self->contacts_.size());
                for (const auto& c : self->contacts_) {
                    rows.push_back({QString::fromStdString(c.contact_type),
                                    QString::fromStdString(c.country_code),
                                    QString::fromStdString(c.street_line_1),
                                    QString::fromStdString(c.city),
                                    QString::fromStdString(c.phone)});
                }
                self->contactTable_->setRows(rows);
            });
    watcher->setFuture(QtConcurrent::run(task));
}

void PartyChildEntityTables::onAddContact() {
    if (readOnly_)
        return;
    if (partyId_.is_nil()) {
        MessageBoxHelper::warning(
            dialogParent_, "Save Required", "Save the party first, then add contact information.");
        return;
    }
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            dialogParent_, "Disconnected", "Cannot add contact while disconnected from server.");
        return;
    }

    QDialog wrapper(dialogParent_);
    wrapper.setWindowTitle("Add Contact");
    auto* layout = new QVBoxLayout(&wrapper);
    layout->setContentsMargins(0, 0, 0, 0);

    auto* dialog = new PartyContactInformationDetailDialog(&wrapper);
    layout->addWidget(dialog);
    dialog->setClientManager(clientManager_);
    dialog->setUsername(username_);
    dialog->setChangeReasonCache(changeReasonCache_);
    dialog->setImageCache(imageCache_);
    dialog->setCreateMode(true);

    refdata::domain::party_contact_information newContact;
    boost::uuids::random_generator uuid_gen;
    newContact.id = uuid_gen();
    newContact.party_id = partyId_;
    dialog->setInformation(newContact);

    connect(dialog,
            &PartyContactInformationDetailDialog::partyContactInformationSaved,
            &wrapper,
            &QDialog::accept);

    if (wrapper.exec() == QDialog::Accepted)
        loadContacts();
}

void PartyChildEntityTables::onDeleteContact(int row) {
    if (!clientManager_ || !clientManager_->isConnected() || readOnly_ || row < 0 ||
        row >= static_cast<int>(contacts_.size()))
        return;

    const auto id = contacts_[static_cast<std::size_t>(row)].id;
    QPointer<PartyChildEntityTables> self = this;
    auto* cm = clientManager_;
    auto task = [cm, id]() -> std::pair<bool, QString> {
        refdata::messaging::delete_party_contact_information_request req;
        req.ids.push_back(boost::uuids::to_string(id));
        auto result = cm->process_authenticated_request(std::move(req));
        if (!result)
            return {false, QString::fromStdString(result.error())};
        return {result->success, QString::fromStdString(result->message)};
    };
    auto* watcher = new QFutureWatcher<std::pair<bool, QString>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, QString>>::finished, this, [self, watcher]() {
        const auto [ok, message] = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        if (ok)
            self->loadContacts();
        else
            MessageBoxHelper::warning(self->dialogParent_, "Delete Failed", message);
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void PartyChildEntityTables::onEditContact(int row) {
    if (row < 0 || row >= static_cast<int>(contacts_.size()))
        return;
    const auto contact = contacts_[static_cast<std::size_t>(row)];

    QDialog wrapper(dialogParent_);
    wrapper.setWindowTitle("Contact Information Details");
    auto* layout = new QVBoxLayout(&wrapper);
    layout->setContentsMargins(0, 0, 0, 0);

    auto* dialog = new PartyContactInformationDetailDialog(&wrapper);
    layout->addWidget(dialog);
    dialog->setClientManager(clientManager_);
    dialog->setUsername(username_);
    dialog->setChangeReasonCache(changeReasonCache_);
    dialog->setImageCache(imageCache_);
    dialog->setCreateMode(false);
    dialog->setReadOnly(readOnly_ || !clientManager_ || !clientManager_->isConnected());
    dialog->setInformation(contact);

    connect(dialog,
            &PartyContactInformationDetailDialog::partyContactInformationSaved,
            &wrapper,
            &QDialog::accept);

    if (wrapper.exec() == QDialog::Accepted)
        loadContacts();
}

}
