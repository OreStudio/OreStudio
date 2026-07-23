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
#include "ores.qt/AccountChildEntityTables.hpp"
#include "ores.iam.api/messaging/account_contact_information_protocol.hpp"
#include "ores.qt/AccountContactInformationDetailDialog.hpp"
#include "ores.qt/ChildEntityTableWidget.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
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

AccountChildEntityTables::AccountChildEntityTables(QWidget* dialogParent)
    : QObject(dialogParent)
    , dialogParent_(dialogParent)
    , contactTable_(new ChildEntityTableWidget({"Full Name", "Country", "Street", "City", "Phone"},
                                               "Add Contact",
                                               "Delete Contact",
                                               dialogParent)) {

    connect(contactTable_,
            &ChildEntityTableWidget::addRequested,
            this,
            &AccountChildEntityTables::onAddContact);
    connect(contactTable_,
            &ChildEntityTableWidget::deleteRequested,
            this,
            &AccountChildEntityTables::onDeleteContact);
    connect(contactTable_->table(),
            &QTableWidget::cellDoubleClicked,
            this,
            [this](int row, int /* column */) { onEditContact(row); });
}

void AccountChildEntityTables::attachTo(QTabWidget* tabWidget) {
    // Insert before the last tab (Provenance) so dynamically-attached
    // tabs never push it out of the last slot.
    tabWidget->insertTab(tabWidget->count() - 1, contactTable_, "Contact Information");
}

void AccountChildEntityTables::reload(const boost::uuids::uuid& accountId,
                                      ClientManager* clientManager,
                                      const std::string& username,
                                      ImageCache* imageCache,
                                      ChangeReasonCache* changeReasonCache) {
    accountId_ = accountId;
    clientManager_ = clientManager;
    username_ = username;
    imageCache_ = imageCache;
    changeReasonCache_ = changeReasonCache;
    if (accountId_.is_nil() || !clientManager_ || !clientManager_->isConnected())
        return;
    loadContacts();
}

void AccountChildEntityTables::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
}

void AccountChildEntityTables::loadContacts() {
    QPointer<AccountChildEntityTables> self = this;
    auto* cm = clientManager_;
    const auto accountIdStr = boost::uuids::to_string(accountId_);

    auto task = [cm, accountIdStr]() -> std::vector<iam::domain::account_contact_information> {
        iam::messaging::get_account_contact_informations_by_account_id_request req;
        req.account_id = accountIdStr;
        req.limit = 1000;
        auto result = cm->process_authenticated_request(std::move(req));
        if (!result || !result->success)
            return {};
        return result->account_contact_informations;
    };

    auto* watcher = new QFutureWatcher<std::vector<iam::domain::account_contact_information>>(this);
    connect(watcher,
            &QFutureWatcher<std::vector<iam::domain::account_contact_information>>::finished,
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
                    rows.push_back({QString::fromStdString(c.full_name),
                                    QString::fromStdString(c.country_code),
                                    QString::fromStdString(c.street_line_1),
                                    QString::fromStdString(c.city),
                                    QString::fromStdString(c.phone)});
                }
                self->contactTable_->setRows(rows);
            });
    watcher->setFuture(QtConcurrent::run(task));
}

void AccountChildEntityTables::onAddContact() {
    if (readOnly_)
        return;
    if (accountId_.is_nil()) {
        MessageBoxHelper::warning(dialogParent_,
                                  "Save Required",
                                  "Save the account first, then add contact information.");
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

    auto* dialog = new AccountContactInformationDetailDialog(&wrapper);
    layout->addWidget(dialog);
    dialog->setClientManager(clientManager_);
    dialog->setChangeReasonCache(changeReasonCache_);
    dialog->setImageCache(imageCache_);
    dialog->setUsername(username_);
    dialog->setCreateMode(true);

    iam::domain::account_contact_information newContact;
    boost::uuids::random_generator uuid_gen;
    newContact.id = uuid_gen();
    newContact.account_id = accountId_;
    dialog->setInformation(newContact);

    connect(dialog,
            &AccountContactInformationDetailDialog::accountContactInformationSaved,
            &wrapper,
            &QDialog::accept);

    if (wrapper.exec() == QDialog::Accepted)
        loadContacts();
}

void AccountChildEntityTables::onDeleteContact(int row) {
    if (!clientManager_ || !clientManager_->isConnected() || readOnly_ || row < 0 ||
        row >= static_cast<int>(contacts_.size()))
        return;

    const auto id = contacts_[static_cast<std::size_t>(row)].id;
    QPointer<AccountChildEntityTables> self = this;
    auto* cm = clientManager_;
    auto task = [cm, id]() -> std::pair<bool, QString> {
        iam::messaging::delete_account_contact_information_request req;
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

void AccountChildEntityTables::onEditContact(int row) {
    if (row < 0 || row >= static_cast<int>(contacts_.size()))
        return;
    const auto contact = contacts_[static_cast<std::size_t>(row)];

    QDialog wrapper(dialogParent_);
    wrapper.setWindowTitle("Contact Information Details");
    auto* layout = new QVBoxLayout(&wrapper);
    layout->setContentsMargins(0, 0, 0, 0);

    auto* dialog = new AccountContactInformationDetailDialog(&wrapper);
    layout->addWidget(dialog);
    dialog->setClientManager(clientManager_);
    dialog->setChangeReasonCache(changeReasonCache_);
    dialog->setImageCache(imageCache_);
    dialog->setUsername(username_);
    dialog->setCreateMode(false);
    dialog->setReadOnly(readOnly_ || !clientManager_ || !clientManager_->isConnected());
    dialog->setInformation(contact);

    connect(dialog,
            &AccountContactInformationDetailDialog::accountContactInformationSaved,
            &wrapper,
            &QDialog::accept);

    if (wrapper.exec() == QDialog::Accepted)
        loadContacts();
}

}
