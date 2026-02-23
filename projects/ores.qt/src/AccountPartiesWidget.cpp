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
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.iam/messaging/account_party_protocol.hpp"
#include "ores.refdata/messaging/party_protocol.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.dq/domain/change_reason_constants.hpp"

namespace ores::qt {

using namespace ores::logging;

AccountPartiesWidget::AccountPartiesWidget(QWidget* parent)
    : QWidget(parent),
      partiesGroup_(new QGroupBox("Assigned Parties", this)),
      assignedList_(new QListWidget(this)),
      partyCombo_(new QComboBox(this)),
      addButton_(new QToolButton(this)),
      removeButton_(new QToolButton(this)),
      saveButton_(new QToolButton(this)) {

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
    addButton_->setToolTip("Stage selected party for addition");
    addButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(addButton_, &QToolButton::clicked,
        this, &AccountPartiesWidget::onAddPartyClicked);

    removeButton_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Delete, IconUtils::DefaultIconColor));
    removeButton_->setToolTip("Stage selected party for removal");
    removeButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(removeButton_, &QToolButton::clicked,
        this, &AccountPartiesWidget::onRemovePartyClicked);

    saveButton_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Save, IconUtils::DefaultIconColor));
    saveButton_->setToolTip("Save pending party changes");
    saveButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(saveButton_, &QToolButton::clicked,
        this, &AccountPartiesWidget::onSaveClicked);

    buttonsLayout->addWidget(partyCombo_);
    buttonsLayout->addWidget(addButton_);
    buttonsLayout->addWidget(removeButton_);
    buttonsLayout->addStretch();
    buttonsLayout->addWidget(saveButton_);

    groupLayout->addLayout(buttonsLayout);

    mainLayout->addWidget(partiesGroup_);
}

void AccountPartiesWidget::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void AccountPartiesWidget::setChangeReasonCache(ChangeReasonCache* cache) {
    changeReasonCache_ = cache;
}

void AccountPartiesWidget::setAccountId(const boost::uuids::uuid& accountId) {
    accountId_ = accountId;
}

bool AccountPartiesWidget::hasPendingChanges() const {
    return !pendingAdds_.empty() || !pendingRemoves_.empty();
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
                self->pendingAdds_.clear();
                self->pendingRemoves_.clear();
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
    if (partyCombo_->count() == 0) return;

    const auto partyIdStr = partyCombo_->currentData().toString().toStdString();
    if (partyIdStr.empty()) return;

    const auto partyId = boost::lexical_cast<boost::uuids::uuid>(partyIdStr);

    // If this party was staged for removal, just un-stage the removal
    auto it = std::find(pendingRemoves_.begin(), pendingRemoves_.end(), partyId);
    if (it != pendingRemoves_.end()) {
        pendingRemoves_.erase(it);
    } else {
        pendingAdds_.push_back(partyId);
    }

    refreshView();
    updateButtonStates();
}

void AccountPartiesWidget::onRemovePartyClicked() {
    auto selected = assignedList_->selectedItems();
    if (selected.isEmpty()) return;

    const auto partyIdStr =
        selected.first()->data(Qt::UserRole).toString().toStdString();
    if (partyIdStr.empty()) return;

    const auto partyId = boost::lexical_cast<boost::uuids::uuid>(partyIdStr);

    // If this party was staged for addition, just un-stage the addition
    auto it = std::find(pendingAdds_.begin(), pendingAdds_.end(), partyId);
    if (it != pendingAdds_.end()) {
        pendingAdds_.erase(it);
    } else {
        pendingRemoves_.push_back(partyId);
    }

    refreshView();
    updateButtonStates();
}

void AccountPartiesWidget::onSaveClicked() {
    if (!hasPendingChanges()) return;

    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Not Connected",
            "Cannot save party changes while disconnected.");
        return;
    }

    // Show change reason dialog
    namespace reason = dq::domain::change_reason_constants;
    std::vector<dq::domain::change_reason> reasons;
    if (changeReasonCache_ && changeReasonCache_->isLoaded()) {
        reasons = changeReasonCache_->getReasonsForAmend(
            std::string{reason::categories::common});
    }

    ChangeReasonDialog dialog(reasons, ChangeReasonDialog::OperationType::Amend,
        true, this);
    if (dialog.exec() != QDialog::Accepted) return;

    const auto changeReasonCode = dialog.selectedReasonCode();
    const auto changeCommentary = dialog.commentary();

    QPointer<AccountPartiesWidget> self = this;
    const auto accountId = accountId_;
    const auto adds = pendingAdds_;
    const auto removes = pendingRemoves_;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this,
        [self, watcher]() {
            auto result = watcher->result();
            watcher->deleteLater();
            if (!self) return;

            if (result.success) {
                emit self->statusMessage("Party changes saved successfully");
                self->loadParties();
                emit self->partiesChanged();
            } else {
                emit self->errorMessage("Save Failed",
                    QString::fromStdString(result.message));
                MessageBoxHelper::critical(self, "Save Failed",
                    QString::fromStdString(result.message));
            }
        });

    QFuture<SaveResult> future =
        QtConcurrent::run([self, accountId, adds, removes,
                           changeReasonCode, changeCommentary]() -> SaveResult {
            if (!self) return {false, "Widget destroyed"};

            // Process adds
            for (const auto& partyId : adds) {
                iam::domain::account_party ap;
                ap.account_id         = accountId;
                ap.party_id           = partyId;
                ap.change_reason_code = changeReasonCode;
                ap.change_commentary  = changeCommentary;
                ap.modified_by        = "";  // server overwrites from session

                iam::messaging::save_account_party_request request;
                request.account_party = ap;

                auto result = self->clientManager_->
                    process_authenticated_request(std::move(request));

                if (!result) {
                    return {false, comms::net::to_string(result.error())};
                }
                if (!result->success) {
                    return {false, result->message};
                }
            }

            // Process removes
            for (const auto& partyId : removes) {
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
                if (result->results.empty() || !result->results.front().success) {
                    const std::string msg = result->results.empty()
                        ? "No result returned"
                        : result->results.front().message;
                    return {false, msg};
                }
            }

            return {true, {}};
        });

    watcher->setFuture(future);
}

void AccountPartiesWidget::onAssignedSelectionChanged() {
    updateButtonStates();
}

void AccountPartiesWidget::refreshView() {
    assignedList_->clear();

    // Build the effective assigned set: DB parties minus removals, plus staged adds
    auto isRemoved = [&](const boost::uuids::uuid& id) {
        return std::find(pendingRemoves_.begin(), pendingRemoves_.end(), id)
            != pendingRemoves_.end();
    };

    auto findPartyName = [&](const boost::uuids::uuid& id) -> QString {
        auto it = std::ranges::find_if(allParties_,
            [&id](const auto& p) { return p.id == id; });
        if (it != allParties_.end()) {
            return QString("%1 (%2)")
                .arg(QString::fromStdString(it->full_name))
                .arg(QString::fromStdString(it->party_type));
        }
        return QString::fromStdString(boost::uuids::to_string(id));
    };

    std::vector<boost::uuids::uuid> effectiveIds;

    // Parties from DB that are not pending removal
    for (const auto& ap : assignedParties_) {
        if (!isRemoved(ap.party_id)) {
            effectiveIds.push_back(ap.party_id);
            auto* item = new QListWidgetItem(findPartyName(ap.party_id));
            item->setData(Qt::UserRole,
                QString::fromStdString(boost::uuids::to_string(ap.party_id)));
            assignedList_->addItem(item);
        }
    }

    // Staged additions
    for (const auto& partyId : pendingAdds_) {
        effectiveIds.push_back(partyId);
        auto* item = new QListWidgetItem(
            findPartyName(partyId) + tr(" [pending]"));
        item->setData(Qt::UserRole,
            QString::fromStdString(boost::uuids::to_string(partyId)));
        assignedList_->addItem(item);
    }

    // Populate combo with parties not in effective set
    partyCombo_->clear();
    for (const auto& party : allParties_) {
        bool inEffective = std::ranges::any_of(effectiveIds,
            [&party](const auto& id) { return id == party.id; });
        if (!inEffective) {
            const QString label = QString("%1 (%2)")
                .arg(QString::fromStdString(party.full_name))
                .arg(QString::fromStdString(party.party_type));
            partyCombo_->addItem(label,
                QVariant(QString::fromStdString(
                    boost::uuids::to_string(party.id))));
        }
    }

    const int total = static_cast<int>(effectiveIds.size());
    const int pending = static_cast<int>(pendingAdds_.size() + pendingRemoves_.size());
    QString title = QString("Assigned Parties (%1)").arg(total);
    if (pending > 0)
        title += QString(" [%1 unsaved]").arg(pending);
    partiesGroup_->setTitle(title);

    updateButtonStates();
}

void AccountPartiesWidget::updateButtonStates() {
    const bool hasSelection = !assignedList_->selectedItems().isEmpty();
    const bool isConnected  = clientManager_ && clientManager_->isConnected();
    const bool hasComboItem = partyCombo_->count() > 0;

    addButton_->setEnabled(!readOnly_ && isConnected && hasComboItem);
    removeButton_->setEnabled(!readOnly_ && isConnected && hasSelection);
    saveButton_->setEnabled(!readOnly_ && isConnected && hasPendingChanges());
}

}
