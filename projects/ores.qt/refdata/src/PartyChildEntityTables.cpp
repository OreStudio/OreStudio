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
#include "ores.refdata.api/messaging/party_contact_information_protocol.hpp"
#include "ores.refdata.api/messaging/party_identifier_protocol.hpp"
#include <QDialog>
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QLineEdit>
#include <QPointer>
#include <QTabWidget>
#include <QtConcurrent/QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

namespace {
constexpr auto default_change_reason = "system.new_record";
}

PartyChildEntityTables::PartyChildEntityTables(QWidget* dialogParent)
    : QObject(dialogParent)
    , dialogParent_(dialogParent)
    , identifierTable_(new ChildEntityTableWidget(
          {"Scheme", "Value", "Description"}, "Add Identifier", "Delete Identifier", dialogParent))
    , contactTable_(new ChildEntityTableWidget(
          {"Type", "Country", "Street", "City", "Phone"}, "Add Contact", "Delete Contact",
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
}

void PartyChildEntityTables::attachTo(QTabWidget* tabWidget) {
    tabWidget->addTab(identifierTable_, "Identifiers");
    tabWidget->addTab(contactTable_, "Contact Information");
}

void PartyChildEntityTables::reload(const boost::uuids::uuid& partyId,
                                    ClientManager* clientManager,
                                    const std::string& username) {
    partyId_ = partyId;
    clientManager_ = clientManager;
    username_ = username;
    if (partyId_.is_nil() || !clientManager_ || !clientManager_->isConnected())
        return;
    loadIdentifiers();
    loadContacts();
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
    if (!clientManager_ || !clientManager_->isConnected() || partyId_.is_nil())
        return;

    QDialog dialog(dialogParent_);
    dialog.setWindowTitle("Add Identifier");
    dialog.setMinimumWidth(400);
    auto* layout = new QFormLayout(&dialog);

    auto* schemeEdit = new QLineEdit(&dialog);
    layout->addRow("Scheme:", schemeEdit);
    auto* valueEdit = new QLineEdit(&dialog);
    layout->addRow("Value:", valueEdit);
    auto* descEdit = new QLineEdit(&dialog);
    layout->addRow("Description:", descEdit);

    auto* buttonBox =
        new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dialog);
    layout->addRow(buttonBox);
    connect(buttonBox, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
    connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);

    if (dialog.exec() != QDialog::Accepted)
        return;
    if (schemeEdit->text().trimmed().isEmpty() || valueEdit->text().trimmed().isEmpty()) {
        MessageBoxHelper::warning(dialogParent_, "Invalid Input", "Scheme and Value are required.");
        return;
    }

    refdata::domain::party_identifier newIdent;
    boost::uuids::random_generator uuid_gen;
    newIdent.id = uuid_gen();
    newIdent.party_id = partyId_;
    newIdent.id_scheme = schemeEdit->text().trimmed().toStdString();
    newIdent.id_value = valueEdit->text().trimmed().toStdString();
    newIdent.description = descEdit->text().trimmed().toStdString();
    newIdent.modified_by = username_;
    newIdent.performed_by = username_;
    newIdent.change_reason_code = default_change_reason;

    QPointer<PartyChildEntityTables> self = this;
    auto* cm = clientManager_;
    auto task = [cm, newIdent]() -> bool {
        refdata::messaging::save_party_identifier_request req;
        req.data = newIdent;
        auto result = cm->process_authenticated_request(std::move(req));
        return result && result->success;
    };
    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        auto ok = watcher->result();
        watcher->deleteLater();
        if (self && ok)
            self->loadIdentifiers();
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void PartyChildEntityTables::onDeleteIdentifier(int row) {
    if (!clientManager_ || !clientManager_->isConnected() || row < 0 ||
        row >= static_cast<int>(identifiers_.size()))
        return;

    const auto id = identifiers_[static_cast<std::size_t>(row)].id;
    QPointer<PartyChildEntityTables> self = this;
    auto* cm = clientManager_;
    auto task = [cm, id]() -> bool {
        refdata::messaging::delete_party_identifier_request req;
        req.ids.push_back(boost::uuids::to_string(id));
        auto result = cm->process_authenticated_request(std::move(req));
        return result && result->success;
    };
    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        auto ok = watcher->result();
        watcher->deleteLater();
        if (self && ok)
            self->loadIdentifiers();
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void PartyChildEntityTables::loadContacts() {
    QPointer<PartyChildEntityTables> self = this;
    auto* cm = clientManager_;
    const auto partyIdStr = boost::uuids::to_string(partyId_);

    auto task = [cm, partyIdStr]() -> std::vector<refdata::domain::party_contact_information> {
        refdata::messaging::get_party_contact_informations_request req;
        req.party_id = partyIdStr;
        auto result = cm->process_authenticated_request(std::move(req));
        if (!result)
            return {};
        return result->contact_informations;
    };

    auto* watcher = new QFutureWatcher<std::vector<refdata::domain::party_contact_information>>(this);
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
    if (!clientManager_ || !clientManager_->isConnected() || partyId_.is_nil())
        return;

    QDialog dialog(dialogParent_);
    dialog.setWindowTitle("Add Contact");
    dialog.setMinimumWidth(400);
    auto* layout = new QFormLayout(&dialog);

    auto* typeEdit = new QLineEdit(&dialog);
    layout->addRow("Type:", typeEdit);
    auto* streetEdit = new QLineEdit(&dialog);
    layout->addRow("Street:", streetEdit);
    auto* cityEdit = new QLineEdit(&dialog);
    layout->addRow("City:", cityEdit);
    auto* countryEdit = new QLineEdit(&dialog);
    layout->addRow("Country Code:", countryEdit);
    auto* phoneEdit = new QLineEdit(&dialog);
    layout->addRow("Phone:", phoneEdit);

    auto* buttonBox =
        new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dialog);
    layout->addRow(buttonBox);
    connect(buttonBox, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
    connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);

    if (dialog.exec() != QDialog::Accepted)
        return;
    if (typeEdit->text().trimmed().isEmpty()) {
        MessageBoxHelper::warning(dialogParent_, "Invalid Input", "Type is required.");
        return;
    }

    refdata::domain::party_contact_information newContact;
    boost::uuids::random_generator uuid_gen;
    newContact.id = uuid_gen();
    newContact.party_id = partyId_;
    newContact.contact_type = typeEdit->text().trimmed().toStdString();
    newContact.street_line_1 = streetEdit->text().trimmed().toStdString();
    newContact.city = cityEdit->text().trimmed().toStdString();
    newContact.country_code = countryEdit->text().trimmed().toStdString();
    newContact.phone = phoneEdit->text().trimmed().toStdString();
    newContact.modified_by = username_;
    newContact.performed_by = username_;
    newContact.change_reason_code = default_change_reason;

    QPointer<PartyChildEntityTables> self = this;
    auto* cm = clientManager_;
    auto task = [cm, newContact]() -> bool {
        refdata::messaging::save_party_contact_information_request req;
        req.data = newContact;
        auto result = cm->process_authenticated_request(std::move(req));
        return result && result->success;
    };
    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        auto ok = watcher->result();
        watcher->deleteLater();
        if (self && ok)
            self->loadContacts();
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void PartyChildEntityTables::onDeleteContact(int row) {
    if (!clientManager_ || !clientManager_->isConnected() || row < 0 ||
        row >= static_cast<int>(contacts_.size()))
        return;

    const auto id = contacts_[static_cast<std::size_t>(row)].id;
    QPointer<PartyChildEntityTables> self = this;
    auto* cm = clientManager_;
    auto task = [cm, id]() -> bool {
        refdata::messaging::delete_party_contact_information_request req;
        req.ids.push_back(boost::uuids::to_string(id));
        auto result = cm->process_authenticated_request(std::move(req));
        return result && result->success;
    };
    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        auto ok = watcher->result();
        watcher->deleteLater();
        if (self && ok)
            self->loadContacts();
    });
    watcher->setFuture(QtConcurrent::run(task));
}

}
