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
#include "ores.qt/AccountPartiesWidget.hpp"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QListWidgetItem>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <algorithm>
#include <future>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.iam/messaging/account_party_protocol.hpp"
#include "ores.refdata/messaging/party_protocol.hpp"
#include "ores.comms/net/client_session.hpp"

namespace ores::qt {

using namespace ores::logging;

AccountPartiesWidget::AccountPartiesWidget(QWidget* parent)
    : QWidget(parent),
      partiesGroup_(new QGroupBox("Assigned Parties", this)),
      assignedList_(new QListWidget(this)),
      partyCombo_(new QComboBox(this)),
      addButton_(new QToolButton(this)),
      removeButton_(new QToolButton(this)) {

    setupUi();
    updateButtonStates();
}

void AccountPartiesWidget::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    auto* groupLayout = new QVBoxLayout(partiesGroup_);

    assignedList_->setAlternatingRowColors(true);
    assignedList_->setSelectionMode(QAbstractItemView::SingleSelection);
    connect(assignedList_, &QListWidget::itemSelectionChanged,
        this, &AccountPartiesWidget::onAssignedSelectionChanged);

    groupLayout->addWidget(assignedList_);

    auto* buttonsLayout = new QHBoxLayout();

    addButton_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Add, IconUtils::DefaultIconColor));
    addButton_->setToolTip("Add selected party to this account");
    addButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(addButton_, &QToolButton::clicked,
        this, &AccountPartiesWidget::onAddPartyClicked);

    removeButton_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Delete, IconUtils::DefaultIconColor));
    removeButton_->setToolTip("Remove selected party from this account");
    removeButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(removeButton_, &QToolButton::clicked,
        this, &AccountPartiesWidget::onRemovePartyClicked);

    buttonsLayout->addWidget(partyCombo_);
    buttonsLayout->addWidget(addButton_);
    buttonsLayout->addWidget(removeButton_);

    groupLayout->addLayout(buttonsLayout);

    mainLayout->addWidget(partiesGroup_);
}

void AccountPartiesWidget::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void AccountPartiesWidget::setAccountId(const boost::uuids::uuid& accountId) {
    accountId_ = accountId;
}

void AccountPartiesWidget::loadParties() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load parties: not connected";
        return;
    }

    if (accountId_.is_nil()) {
        BOOST_LOG_SEV(lg(), debug) << "No account ID set, not loading parties";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Loading parties for account: "
                               << boost::uuids::to_string(accountId_);

    QPointer<AccountPartiesWidget> self = this;
    const auto accountId = accountId_;

    struct LoadResult {
        bool success;
        std::vector<iam::domain::account_party> assignedParties;
        std::vector<refdata::domain::party>     allParties;
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this,
        [self, watcher]() {
            auto result = watcher->result();
            watcher->deleteLater();

            if (!self) return;

            if (result.success) {
                self->assignedParties_ = std::move(result.assignedParties);
                self->allParties_      = std::move(result.allParties);
                self->refreshView();
                BOOST_LOG_SEV(lg(), debug)
                    << "Loaded " << self->assignedParties_.size()
                    << " assigned parties";
            } else {
                emit self->errorMessage("Load Failed", "Failed to load parties");
            }
        });

    QFuture<LoadResult> future =
        QtConcurrent::run([self, accountId]() -> LoadResult {
            if (!self) return {false, {}, {}};

            auto assignedFuture = std::async(std::launch::async,
                [&self, &accountId]() {
                    iam::messaging::get_account_parties_by_account_request request;
                    request.account_id = accountId;
                    return self->clientManager_->
                        process_authenticated_request(std::move(request));
                });

            auto allPartiesFuture = std::async(std::launch::async,
                [&self]() {
                    refdata::messaging::get_parties_request request;
                    request.offset = 0;
                    request.limit  = 1000;
                    return self->clientManager_->
                        process_authenticated_request(std::move(request));
                });

            auto assignedResult   = assignedFuture.get();
            auto allPartiesResult = allPartiesFuture.get();

            if (!assignedResult) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to fetch assigned parties: "
                    << comms::net::to_string(assignedResult.error());
                return {false, {}, {}};
            }

            if (!allPartiesResult) {
                BOOST_LOG_SEV(lg(), error)
                    << "Failed to fetch all parties: "
                    << comms::net::to_string(allPartiesResult.error());
                return {false, {}, {}};
            }

            return {true,
                std::move(assignedResult->account_parties),
                std::move(allPartiesResult->parties)};
        });

    watcher->setFuture(future);
}

void AccountPartiesWidget::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    updateButtonStates();
}

void AccountPartiesWidget::onAddPartyClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Not Connected",
            "Cannot add party while disconnected.");
        return;
    }

    if (partyCombo_->count() == 0) {
        MessageBoxHelper::information(this, "No Parties Available",
            "All available parties are already assigned to this account.");
        return;
    }

    const auto partyId = boost::lexical_cast<boost::uuids::uuid>(
        partyCombo_->currentData().toString().toStdString());
    const auto accountId = accountId_;

    BOOST_LOG_SEV(lg(), info) << "Adding party "
                              << boost::uuids::to_string(partyId)
                              << " to account "
                              << boost::uuids::to_string(accountId_);

    QPointer<AccountPartiesWidget> self = this;

    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished,
        this, [self, watcher]() {
            auto [success, message] = watcher->result();
            watcher->deleteLater();

            if (!self) return;

            if (success) {
                emit self->statusMessage("Party added successfully");
                self->loadParties();
                emit self->partiesChanged();
            } else {
                emit self->errorMessage("Add Party Failed",
                    QString::fromStdString(message));
                MessageBoxHelper::critical(self, "Add Party Failed",
                    QString::fromStdString(message));
            }
        });

    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([self, accountId, partyId]()
            -> std::pair<bool, std::string> {
            if (!self) return {false, "Widget destroyed"};

            iam::domain::account_party ap;
            ap.account_id         = accountId;
            ap.party_id           = partyId;
            ap.change_reason_code = "account_management";
            ap.modified_by        = "";  // server overwrites from session

            iam::messaging::save_account_party_request request;
            request.account_party = ap;

            auto result = self->clientManager_->
                process_authenticated_request(std::move(request));

            if (!result) {
                return {false, comms::net::to_string(result.error())};
            }
            return {result->success, result->message};
        });

    watcher->setFuture(future);
}

void AccountPartiesWidget::onRemovePartyClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Not Connected",
            "Cannot remove party while disconnected.");
        return;
    }

    auto selected = assignedList_->selectedItems();
    if (selected.isEmpty()) return;

    const int row = assignedList_->row(selected.first());
    if (row < 0 || row >= static_cast<int>(assignedParties_.size())) return;

    const auto partyId  = assignedParties_[row].party_id;
    const auto accountId = accountId_;

    auto reply = MessageBoxHelper::question(this, "Remove Party",
        "Are you sure you want to remove this party from the account?",
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    BOOST_LOG_SEV(lg(), info) << "Removing party "
                              << boost::uuids::to_string(partyId)
                              << " from account "
                              << boost::uuids::to_string(accountId_);

    QPointer<AccountPartiesWidget> self = this;

    auto* watcher = new QFutureWatcher<std::pair<bool, std::string>>(this);
    connect(watcher, &QFutureWatcher<std::pair<bool, std::string>>::finished,
        this, [self, watcher]() {
            auto [success, message] = watcher->result();
            watcher->deleteLater();

            if (!self) return;

            if (success) {
                emit self->statusMessage("Party removed successfully");
                self->loadParties();
                emit self->partiesChanged();
            } else {
                emit self->errorMessage("Remove Party Failed",
                    QString::fromStdString(message));
                MessageBoxHelper::critical(self, "Remove Party Failed",
                    QString::fromStdString(message));
            }
        });

    QFuture<std::pair<bool, std::string>> future =
        QtConcurrent::run([self, accountId, partyId]()
            -> std::pair<bool, std::string> {
            if (!self) return {false, "Widget destroyed"};

            iam::messaging::delete_account_party_request request;
            iam::messaging::account_party_key key;
            key.account_id = accountId;
            key.party_id   = partyId;
            request.keys.push_back(key);

            auto result = self->clientManager_->
                process_authenticated_request(std::move(request));

            if (!result) {
                return {false, comms::net::to_string(result.error())};
            }

            if (result->results.empty()) {
                return {false, "No result returned from server"};
            }
            return {result->results.front().success,
                    result->results.front().message};
        });

    watcher->setFuture(future);
}

void AccountPartiesWidget::onAssignedSelectionChanged() {
    updateButtonStates();
}

void AccountPartiesWidget::refreshView() {
    // Populate the assigned list
    assignedList_->clear();
    for (const auto& ap : assignedParties_) {
        // Find party name from allParties_
        auto it = std::ranges::find_if(allParties_,
            [&ap](const auto& p) { return p.id == ap.party_id; });

        QString label;
        if (it != allParties_.end()) {
            label = QString("%1 (%2)")
                .arg(QString::fromStdString(it->full_name))
                .arg(QString::fromStdString(it->party_type));
        } else {
            label = QString::fromStdString(
                boost::uuids::to_string(ap.party_id));
        }
        assignedList_->addItem(new QListWidgetItem(label));
    }

    // Populate the combo with unassigned parties
    partyCombo_->clear();
    for (const auto& party : allParties_) {
        bool alreadyAssigned = std::ranges::any_of(assignedParties_,
            [&party](const auto& ap) { return ap.party_id == party.id; });

        if (!alreadyAssigned) {
            const QString label = QString("%1 (%2)")
                .arg(QString::fromStdString(party.full_name))
                .arg(QString::fromStdString(party.party_type));
            partyCombo_->addItem(label,
                QVariant(QString::fromStdString(
                    boost::uuids::to_string(party.id))));
        }
    }

    partiesGroup_->setTitle(
        QString("Assigned Parties (%1)").arg(assignedParties_.size()));

    updateButtonStates();
}

void AccountPartiesWidget::updateButtonStates() {
    const bool hasSelection = !assignedList_->selectedItems().isEmpty();
    const bool isConnected  = clientManager_ && clientManager_->isConnected();
    const bool hasComboItem = partyCombo_->count() > 0;

    addButton_->setEnabled(!readOnly_ && isConnected && hasComboItem);
    removeButton_->setEnabled(!readOnly_ && isConnected && hasSelection);
}

}
