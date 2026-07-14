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
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/party_contact_information_protocol.hpp"
#include "ores.refdata.api/messaging/party_identifier_protocol.hpp"
#include <QComboBox>
#include <QDialog>
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QFutureWatcher>
#include <QLineEdit>
#include <QPointer>
#include <QTableWidget>
#include <QTabWidget>
#include <QtConcurrent/QtConcurrent>
#include <algorithm>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

namespace {
constexpr auto default_change_reason = "system.new_record";

void populate_code_combo(QComboBox* combo, const std::vector<std::string>& codes,
                         const std::string& current = {}) {
    combo->clear();
    for (const auto& code : codes)
        combo->addItem(QString::fromStdString(code));
    if (!current.empty()) {
        const auto idx = combo->findText(QString::fromStdString(current));
        if (idx >= 0)
            combo->setCurrentIndex(idx);
    }
}
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
                                    ImageCache* imageCache) {
    partyId_ = partyId;
    clientManager_ = clientManager;
    username_ = username;
    imageCache_ = imageCache;
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
    if (!clientManager_ || !clientManager_->isConnected() || partyId_.is_nil() || readOnly_)
        return;

    QDialog dialog(dialogParent_);
    dialog.setWindowTitle("Add Identifier");
    dialog.setMinimumWidth(400);
    auto* layout = new QFormLayout(&dialog);

    auto* schemeCombo = new QComboBox(&dialog);
    layout->addRow("Scheme:", schemeCombo);
    auto* valueEdit = new QLineEdit(&dialog);
    layout->addRow("Value:", valueEdit);
    auto* descEdit = new QLineEdit(&dialog);
    layout->addRow("Description:", descEdit);

    auto* buttonBox =
        new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dialog);
    layout->addRow(buttonBox);
    connect(buttonBox, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
    connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);

    auto* cm = clientManager_;
    auto* schemeWatcher = new QFutureWatcher<std::vector<std::string>>(&dialog);
    connect(schemeWatcher,
            &QFutureWatcher<std::vector<std::string>>::finished,
            &dialog,
            [schemeWatcher, schemeCombo]() {
                populate_code_combo(schemeCombo, schemeWatcher->result());
                schemeWatcher->deleteLater();
            });
    schemeWatcher->setFuture(QtConcurrent::run([cm]() { return fetch_party_id_scheme_codes(cm); }));

    if (dialog.exec() != QDialog::Accepted)
        return;
    if (schemeCombo->currentText().trimmed().isEmpty() || valueEdit->text().trimmed().isEmpty()) {
        MessageBoxHelper::warning(dialogParent_, "Invalid Input", "Scheme and Value are required.");
        return;
    }

    refdata::domain::party_identifier newIdent;
    boost::uuids::random_generator uuid_gen;
    newIdent.id = uuid_gen();
    newIdent.party_id = partyId_;
    newIdent.id_scheme = schemeCombo->currentText().trimmed().toStdString();
    newIdent.id_value = valueEdit->text().trimmed().toStdString();
    newIdent.description = descEdit->text().trimmed().toStdString();
    newIdent.modified_by = username_;
    newIdent.performed_by = username_;
    newIdent.change_reason_code = default_change_reason;

    QPointer<PartyChildEntityTables> self = this;
    auto task = [cm, newIdent]() -> std::pair<bool, QString> {
        refdata::messaging::save_party_identifier_request req;
        req.data = newIdent;
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
            MessageBoxHelper::warning(self->dialogParent_, "Save Failed", message);
    });
    watcher->setFuture(QtConcurrent::run(task));
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
    const bool editable = !readOnly_ && clientManager_ && clientManager_->isConnected();

    QDialog dialog(dialogParent_);
    dialog.setWindowTitle("Identifier Details");
    dialog.setMinimumWidth(400);
    auto* layout = new QFormLayout(&dialog);

    auto* schemeCombo = new QComboBox(&dialog);
    schemeCombo->setEnabled(editable);
    layout->addRow("Scheme:", schemeCombo);
    auto* valueEdit = new QLineEdit(QString::fromStdString(ident.id_value), &dialog);
    valueEdit->setReadOnly(!editable);
    layout->addRow("Value:", valueEdit);
    auto* descEdit = new QLineEdit(QString::fromStdString(ident.description), &dialog);
    descEdit->setReadOnly(!editable);
    layout->addRow("Description:", descEdit);

    const auto buttons =
        editable ? QDialogButtonBox::Save | QDialogButtonBox::Cancel : QDialogButtonBox::Close;
    auto* buttonBox = new QDialogButtonBox(buttons, &dialog);
    layout->addRow(buttonBox);
    connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);
    if (editable)
        connect(buttonBox, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);

    auto* cm = clientManager_;
    auto* schemeWatcher = new QFutureWatcher<std::vector<std::string>>(&dialog);
    connect(schemeWatcher,
            &QFutureWatcher<std::vector<std::string>>::finished,
            &dialog,
            [schemeWatcher, schemeCombo, currentScheme = ident.id_scheme]() {
                populate_code_combo(schemeCombo, schemeWatcher->result(), currentScheme);
                schemeWatcher->deleteLater();
            });
    schemeWatcher->setFuture(QtConcurrent::run([cm]() { return fetch_party_id_scheme_codes(cm); }));

    if (dialog.exec() != QDialog::Accepted || !editable)
        return;

    auto updated = ident;
    updated.id_scheme = schemeCombo->currentText().trimmed().toStdString();
    updated.id_value = valueEdit->text().trimmed().toStdString();
    updated.description = descEdit->text().trimmed().toStdString();
    updated.modified_by = username_;
    updated.performed_by = username_;
    updated.change_reason_code = default_change_reason;

    QPointer<PartyChildEntityTables> self = this;
    auto task = [cm, updated]() -> std::pair<bool, QString> {
        refdata::messaging::save_party_identifier_request req;
        req.data = updated;
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
            MessageBoxHelper::warning(self->dialogParent_, "Save Failed", message);
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
    if (!clientManager_ || !clientManager_->isConnected() || partyId_.is_nil() || readOnly_)
        return;

    QDialog dialog(dialogParent_);
    dialog.setWindowTitle("Add Contact");
    dialog.setMinimumWidth(400);
    auto* layout = new QFormLayout(&dialog);

    auto* typeCombo = new QComboBox(&dialog);
    layout->addRow("Type:", typeCombo);
    auto* streetEdit = new QLineEdit(&dialog);
    layout->addRow("Street:", streetEdit);
    auto* cityEdit = new QLineEdit(&dialog);
    layout->addRow("City:", cityEdit);
    auto* countryCombo = new QComboBox(&dialog);
    layout->addRow("Country Code:", countryCombo);
    auto* phoneEdit = new QLineEdit(&dialog);
    layout->addRow("Phone:", phoneEdit);

    auto* buttonBox =
        new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dialog);
    layout->addRow(buttonBox);
    connect(buttonBox, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
    connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);

    auto* cm = clientManager_;
    auto* typeWatcher = new QFutureWatcher<std::vector<std::string>>(&dialog);
    connect(typeWatcher,
            &QFutureWatcher<std::vector<std::string>>::finished,
            &dialog,
            [typeWatcher, typeCombo]() {
                populate_code_combo(typeCombo, typeWatcher->result());
                typeWatcher->deleteLater();
            });
    typeWatcher->setFuture(QtConcurrent::run([cm]() { return fetch_contact_type_codes(cm); }));

    auto* imageCache = imageCache_;
    countryCombo->setIconSize(single_flag_icon_size());
    auto* countryWatcher = new QFutureWatcher<std::vector<std::string>>(&dialog);
    connect(countryWatcher,
            &QFutureWatcher<std::vector<std::string>>::finished,
            &dialog,
            [countryWatcher, countryCombo, imageCache]() {
                populate_code_combo(countryCombo, countryWatcher->result());
                apply_flag_icons(countryCombo, imageCache, FlagSource::Country);
                countryWatcher->deleteLater();
            });
    countryWatcher->setFuture(QtConcurrent::run([cm]() { return fetch_country_codes(cm); }));

    if (dialog.exec() != QDialog::Accepted)
        return;
    if (typeCombo->currentText().trimmed().isEmpty()) {
        MessageBoxHelper::warning(dialogParent_, "Invalid Input", "Type is required.");
        return;
    }

    refdata::domain::party_contact_information newContact;
    boost::uuids::random_generator uuid_gen;
    newContact.id = uuid_gen();
    newContact.party_id = partyId_;
    newContact.contact_type = typeCombo->currentText().trimmed().toStdString();
    newContact.street_line_1 = streetEdit->text().trimmed().toStdString();
    newContact.city = cityEdit->text().trimmed().toStdString();
    newContact.country_code = countryCombo->currentText().trimmed().toStdString();
    newContact.phone = phoneEdit->text().trimmed().toStdString();
    newContact.modified_by = username_;
    newContact.performed_by = username_;
    newContact.change_reason_code = default_change_reason;

    QPointer<PartyChildEntityTables> self = this;
    auto task = [cm, newContact]() -> std::pair<bool, QString> {
        refdata::messaging::save_party_contact_information_request req;
        req.data = newContact;
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
            MessageBoxHelper::warning(self->dialogParent_, "Save Failed", message);
    });
    watcher->setFuture(QtConcurrent::run(task));
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
    const bool editable = !readOnly_ && clientManager_ && clientManager_->isConnected();

    QDialog dialog(dialogParent_);
    dialog.setWindowTitle("Contact Information Details");
    dialog.setMinimumWidth(500);
    auto* layout = new QFormLayout(&dialog);

    auto* typeCombo = new QComboBox(&dialog);
    typeCombo->setEnabled(editable);
    auto* streetLine1Edit = new QLineEdit(QString::fromStdString(contact.street_line_1), &dialog);
    auto* streetLine2Edit = new QLineEdit(QString::fromStdString(contact.street_line_2), &dialog);
    auto* cityEdit = new QLineEdit(QString::fromStdString(contact.city), &dialog);
    auto* stateEdit = new QLineEdit(QString::fromStdString(contact.state), &dialog);
    auto* countryCombo = new QComboBox(&dialog);
    countryCombo->setEnabled(editable);
    auto* postalEdit = new QLineEdit(QString::fromStdString(contact.postal_code), &dialog);
    auto* phoneEdit = new QLineEdit(QString::fromStdString(contact.phone), &dialog);
    auto* emailEdit = new QLineEdit(QString::fromStdString(contact.email), &dialog);
    auto* webEdit = new QLineEdit(QString::fromStdString(contact.web_page), &dialog);
    for (auto* edit : {streetLine1Edit, streetLine2Edit, cityEdit, stateEdit,
                       postalEdit, phoneEdit, emailEdit, webEdit})
        edit->setReadOnly(!editable);

    layout->addRow("Type:", typeCombo);
    layout->addRow("Street Line 1:", streetLine1Edit);
    layout->addRow("Street Line 2:", streetLine2Edit);
    layout->addRow("City:", cityEdit);
    layout->addRow("State:", stateEdit);
    layout->addRow("Country Code:", countryCombo);
    layout->addRow("Postal Code:", postalEdit);
    layout->addRow("Phone:", phoneEdit);
    layout->addRow("Email:", emailEdit);
    layout->addRow("Web Page:", webEdit);

    const auto buttons =
        editable ? QDialogButtonBox::Save | QDialogButtonBox::Cancel : QDialogButtonBox::Close;
    auto* buttonBox = new QDialogButtonBox(buttons, &dialog);
    layout->addRow(buttonBox);
    connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);
    if (editable)
        connect(buttonBox, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);

    auto* cm = clientManager_;
    auto* typeWatcher = new QFutureWatcher<std::vector<std::string>>(&dialog);
    connect(typeWatcher,
            &QFutureWatcher<std::vector<std::string>>::finished,
            &dialog,
            [typeWatcher, typeCombo, currentType = contact.contact_type]() {
                populate_code_combo(typeCombo, typeWatcher->result(), currentType);
                typeWatcher->deleteLater();
            });
    typeWatcher->setFuture(QtConcurrent::run([cm]() { return fetch_contact_type_codes(cm); }));

    auto* imageCache = imageCache_;
    countryCombo->setIconSize(single_flag_icon_size());
    auto* countryWatcher = new QFutureWatcher<std::vector<std::string>>(&dialog);
    connect(countryWatcher,
            &QFutureWatcher<std::vector<std::string>>::finished,
            &dialog,
            [countryWatcher, countryCombo, imageCache, currentCountry = contact.country_code]() {
                populate_code_combo(countryCombo, countryWatcher->result(), currentCountry);
                apply_flag_icons(countryCombo, imageCache, FlagSource::Country);
                countryWatcher->deleteLater();
            });
    countryWatcher->setFuture(QtConcurrent::run([cm]() { return fetch_country_codes(cm); }));

    if (dialog.exec() != QDialog::Accepted || !editable)
        return;

    auto updated = contact;
    updated.contact_type = typeCombo->currentText().trimmed().toStdString();
    updated.street_line_1 = streetLine1Edit->text().trimmed().toStdString();
    updated.street_line_2 = streetLine2Edit->text().trimmed().toStdString();
    updated.city = cityEdit->text().trimmed().toStdString();
    updated.state = stateEdit->text().trimmed().toStdString();
    updated.country_code = countryCombo->currentText().trimmed().toStdString();
    updated.postal_code = postalEdit->text().trimmed().toStdString();
    updated.phone = phoneEdit->text().trimmed().toStdString();
    updated.email = emailEdit->text().trimmed().toStdString();
    updated.web_page = webEdit->text().trimmed().toStdString();
    updated.modified_by = username_;
    updated.performed_by = username_;
    updated.change_reason_code = default_change_reason;

    QPointer<PartyChildEntityTables> self = this;
    auto task = [cm, updated]() -> std::pair<bool, QString> {
        refdata::messaging::save_party_contact_information_request req;
        req.data = updated;
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
            MessageBoxHelper::warning(self->dialogParent_, "Save Failed", message);
    });
    watcher->setFuture(QtConcurrent::run(task));
}

}
