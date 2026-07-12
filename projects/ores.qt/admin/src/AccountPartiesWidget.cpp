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
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QListWidgetItem>
#include <QSignalBlocker>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <future>

namespace ores::qt {

using namespace ores::logging;

AccountPartiesWidget::AccountPartiesWidget(QWidget* parent)
    : QWidget(parent)
    , partiesGroup_(new QGroupBox("Assigned Parties", this))
    , assignedList_(new QListWidget(this))
    , partyCombo_(new QComboBox(this))
    , addButton_(new QToolButton(this))
    , removeButton_(new QToolButton(this))
    , defaultPartyLabel_(new QLabel("Default Party:", this))
    , defaultPartyCombo_(new QComboBox(this)) {

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
    connect(assignedList_,
            &QListWidget::itemSelectionChanged,
            this,
            &AccountPartiesWidget::onAssignedSelectionChanged);

    groupLayout->addWidget(assignedList_);

    auto* buttonsLayout = new QHBoxLayout();

    addButton_->setIcon(IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor));
    addButton_->setToolTip("Add selected party");
    addButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(addButton_, &QToolButton::clicked, this, &AccountPartiesWidget::onAddPartyClicked);

    removeButton_->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
    removeButton_->setToolTip("Remove selected party");
    removeButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(
        removeButton_, &QToolButton::clicked, this, &AccountPartiesWidget::onRemovePartyClicked);

    buttonsLayout->addWidget(partyCombo_);
    buttonsLayout->addWidget(addButton_);
    buttonsLayout->addWidget(removeButton_);
    buttonsLayout->addStretch();

    groupLayout->addLayout(buttonsLayout);

    mainLayout->addWidget(partiesGroup_);

    // Default party — just below the assignment list/controls. Options are
    // limited to already-persisted assignments (add a party, save, then set
    // it as default in a later save — avoids racing the add against the
    // membership check on the same save).
    auto* defaultPartyLayout = new QHBoxLayout();
    defaultPartyCombo_->setToolTip(
        "Party to log into automatically when the login dialog's quick-login checkbox is "
        "ticked. Only parties already assigned to this account (and already saved) can be "
        "selected.");
    connect(defaultPartyCombo_,
            &QComboBox::currentIndexChanged,
            this,
            &AccountPartiesWidget::defaultPartyChanged);
    defaultPartyLayout->addWidget(defaultPartyLabel_);
    defaultPartyLayout->addWidget(defaultPartyCombo_, 1);
    mainLayout->addLayout(defaultPartyLayout);
}

void AccountPartiesWidget::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void AccountPartiesWidget::setAccountId(const boost::uuids::uuid& accountId) {
    accountId_ = accountId;
}

void AccountPartiesWidget::setDefaultPartyId(const boost::uuids::uuid& defaultPartyId) {
    defaultPartyId_ = defaultPartyId;
    defaultPartyComboInitialized_ = false;
    refreshView();
}

void AccountPartiesWidget::rebaseDefaultParty(const boost::uuids::uuid& defaultPartyId) {
    defaultPartyId_ = defaultPartyId;
    refreshView();
}

void AccountPartiesWidget::setAccountType(const std::string& accountType) {
    accountType_ = accountType;
}

bool AccountPartiesWidget::hasPendingChanges() const {
    return !pendingAdds_.empty() || !pendingRemoves_.empty();
}

bool AccountPartiesWidget::hasAvailableParties() const {
    return !allParties_.empty();
}

const std::vector<boost::uuids::uuid>& AccountPartiesWidget::pendingAdds() const {
    return pendingAdds_;
}

const std::vector<boost::uuids::uuid>& AccountPartiesWidget::pendingRemoves() const {
    return pendingRemoves_;
}

const std::vector<iam::domain::account_party>& AccountPartiesWidget::assignedParties() const {
    return assignedParties_;
}

const std::vector<refdata::domain::party>& AccountPartiesWidget::allParties() const {
    return allParties_;
}

boost::uuids::uuid AccountPartiesWidget::selectedDefaultPartyId() const {
    const auto str = defaultPartyCombo_->currentData().toString().toStdString();
    if (str.empty())
        return boost::uuids::nil_uuid();
    return boost::lexical_cast<boost::uuids::uuid>(str);
}

bool AccountPartiesWidget::hasPendingDefaultPartyChange() const {
    // Before the combo has been seeded from the account's actual stored
    // default (populateDefaultPartyCombo(), called once load() resolves),
    // selectedDefaultPartyId() is nil regardless of what the real default
    // is — comparing against it here would spuriously report a pending
    // change (and, if saved while still nil, would wipe a real default).
    if (!defaultPartyComboInitialized_)
        return false;
    return selectedDefaultPartyId() != defaultPartyId_;
}

bool AccountPartiesWidget::isDefaultPartyReady() const {
    return defaultPartyComboInitialized_;
}

void AccountPartiesWidget::load() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load parties: not connected";
        return;
    }

    const bool has_account = !accountId_.is_nil();
    if (has_account) {
        BOOST_LOG_SEV(lg(), debug)
            << "Loading parties for account: " << boost::uuids::to_string(accountId_);
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Loading available parties for new account";
    }

    QPointer<AccountPartiesWidget> self = this;
    const auto accountId = accountId_;

    struct LoadResult {
        bool success;
        std::vector<iam::domain::account_party> assignedParties;
        std::vector<refdata::domain::party> allParties;
    };

    auto* watcher = new QFutureWatcher<LoadResult>(this);
    connect(watcher, &QFutureWatcher<LoadResult>::finished, this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        if (result.success) {
            self->assignedParties_ = std::move(result.assignedParties);
            self->allParties_ = std::move(result.allParties);
            self->pendingAdds_.clear();
            self->pendingRemoves_.clear();
            self->refreshView();
            self->populateDefaultPartyCombo();
            BOOST_LOG_SEV(lg(), debug)
                << "Loaded " << self->assignedParties_.size() << " assigned, "
                << self->allParties_.size() << " total parties";
            emit self->dataLoaded();
        } else {
            emit self->errorMessage("Load Failed", "Failed to load parties");
        }
    });

    auto* clientManager = clientManager_;
    QFuture<LoadResult> future =
        QtConcurrent::run([self, clientManager, accountId, has_account]() -> LoadResult {
            if (!self)
                return {.success = false};

            std::vector<iam::domain::account_party> assigned;
            if (has_account) {
                auto assignedFuture = std::async(std::launch::async, [clientManager, accountId]() {
                    iam::messaging::get_account_parties_by_account_request request;
                    request.account_id = boost::uuids::to_string(accountId);
                    return clientManager->process_authenticated_request(std::move(request));
                });

                auto allPartiesFuture = std::async(std::launch::async, [clientManager]() {
                    refdata::messaging::get_parties_request request;
                    request.offset = 0;
                    request.limit = 1000;
                    return clientManager->process_authenticated_request(std::move(request));
                });

                auto assignedResult = assignedFuture.get();
                auto allPartiesResult = allPartiesFuture.get();

                if (!assignedResult) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Failed to fetch assigned parties: " << assignedResult.error();
                    return {.success = false};
                }
                if (!allPartiesResult) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Failed to fetch all parties: " << allPartiesResult.error();
                    return {.success = false};
                }
                return {.success = true,
                        .assignedParties = std::move(assignedResult->account_parties),
                        .allParties = std::move(allPartiesResult->parties)};
            }

            // Create mode: only fetch all parties
            refdata::messaging::get_parties_request request;
            request.offset = 0;
            request.limit = 1000;
            auto result = clientManager->process_authenticated_request(std::move(request));

            if (!result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to fetch parties: " << result.error();
                return {.success = false};
            }
            return {.success = true, .allParties = std::move(result->parties)};
        });

    watcher->setFuture(future);
}

void AccountPartiesWidget::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    defaultPartyCombo_->setEnabled(!readOnly);
    updateButtonStates();
}

void AccountPartiesWidget::onAddPartyClicked() {
    if (partyCombo_->count() == 0)
        return;

    // Warn if this is a tenant admin account — party assignment is unusual
    if (accountType_ == "admin" || accountType_ == "tenant_admin") {
        const auto reply = MessageBoxHelper::question(
            this,
            "Administrator Account",
            "This is an administrator account. Assigning parties to administrator "
            "accounts is not typical and may not be appropriate.\n\nContinue?",
            QMessageBox::Yes | QMessageBox::No);
        if (reply != QMessageBox::Yes)
            return;
    }

    const auto partyIdStr = partyCombo_->currentData().toString().toStdString();
    if (partyIdStr.empty())
        return;

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
    emit partyListChanged();
}

void AccountPartiesWidget::onRemovePartyClicked() {
    auto selected = assignedList_->selectedItems();
    if (selected.isEmpty())
        return;

    const auto partyIdStr = selected.first()->data(Qt::UserRole).toString().toStdString();
    if (partyIdStr.empty())
        return;

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
    emit partyListChanged();
}

void AccountPartiesWidget::onAssignedSelectionChanged() {
    updateButtonStates();
}

namespace {

// Deep indigo — reads clearly on the app's dark theme as "highlighted/
// special" without the alarm connotations of yellow (warning) or red (error).
const QColor default_party_accent_color{124, 108, 232};

}

void AccountPartiesWidget::refreshView() {
    assignedList_->clear();

    // Build the effective assigned set: DB parties minus removals, plus staged adds
    auto isRemoved = [&](const boost::uuids::uuid& id) {
        return std::find(pendingRemoves_.begin(), pendingRemoves_.end(), id) !=
               pendingRemoves_.end();
    };

    auto findPartyName = [&](const boost::uuids::uuid& id) -> QString {
        auto it = std::ranges::find_if(allParties_, [&id](const auto& p) { return p.id == id; });
        if (it != allParties_.end()) {
            return QString("%1 (%2)")
                .arg(QString::fromStdString(it->full_name))
                .arg(QString::fromStdString(it->party_type));
        }
        return QString::fromStdString(boost::uuids::to_string(id));
    };

    auto makeItem = [&](const boost::uuids::uuid& id) {
        auto* item = new QListWidgetItem(findPartyName(id));
        item->setData(Qt::UserRole, QString::fromStdString(boost::uuids::to_string(id)));
        if (!defaultPartyId_.is_nil() && id == defaultPartyId_) {
            item->setText("★ " + item->text() + " — Default");
            item->setForeground(default_party_accent_color);
            QFont font = item->font();
            font.setBold(true);
            item->setFont(font);
            item->setToolTip("This is the account's default quick-login party.");
        }
        return item;
    };

    auto partyName = [&](const boost::uuids::uuid& id) -> QString {
        auto it = std::ranges::find_if(allParties_, [&id](const auto& p) { return p.id == id; });
        return it != allParties_.end() ? QString::fromStdString(it->full_name) :
                                         QString::fromStdString(boost::uuids::to_string(id));
    };

    std::vector<boost::uuids::uuid> effectiveIds;

    // Sort so the default party (if assigned) always appears first — it
    // shouldn't be buried alphabetically in a long counterparty list.
    // Everything else sorts alphabetically by party name.
    std::vector<iam::domain::account_party> orderedAssigned;
    for (const auto& ap : assignedParties_) {
        if (!isRemoved(ap.party_id))
            orderedAssigned.push_back(ap);
    }
    std::ranges::sort(orderedAssigned, [&](const auto& a, const auto& b) {
        const bool a_default = !defaultPartyId_.is_nil() && a.party_id == defaultPartyId_;
        const bool b_default = !defaultPartyId_.is_nil() && b.party_id == defaultPartyId_;
        if (a_default != b_default)
            return a_default;
        return partyName(a.party_id).compare(partyName(b.party_id), Qt::CaseInsensitive) < 0;
    });

    for (const auto& ap : orderedAssigned) {
        effectiveIds.push_back(ap.party_id);
        assignedList_->addItem(makeItem(ap.party_id));
    }

    // Staged additions, alphabetical
    auto orderedAdds = pendingAdds_;
    std::ranges::sort(orderedAdds, [&](const auto& a, const auto& b) {
        return partyName(a).compare(partyName(b), Qt::CaseInsensitive) < 0;
    });
    for (const auto& partyId : orderedAdds) {
        effectiveIds.push_back(partyId);
        assignedList_->addItem(makeItem(partyId));
    }

    // Populate combo with parties not in effective set, alphabetical
    auto orderedAvailable = allParties_;
    std::ranges::sort(orderedAvailable, [](const auto& a, const auto& b) {
        return QString::fromStdString(a.full_name)
                   .compare(QString::fromStdString(b.full_name), Qt::CaseInsensitive) < 0;
    });
    partyCombo_->clear();
    for (const auto& party : orderedAvailable) {
        bool inEffective =
            std::ranges::any_of(effectiveIds, [&party](const auto& id) { return id == party.id; });
        if (!inEffective) {
            const QString label = QString("%1 (%2)")
                                      .arg(QString::fromStdString(party.full_name))
                                      .arg(QString::fromStdString(party.party_type));
            partyCombo_->addItem(
                label, QVariant(QString::fromStdString(boost::uuids::to_string(party.id))));
        }
    }

    partiesGroup_->setTitle(
        QString("Assigned Parties (%1)").arg(static_cast<int>(effectiveIds.size())));

    updateButtonStates();
}

void AccountPartiesWidget::populateDefaultPartyCombo() {
    const QSignalBlocker blocker(defaultPartyCombo_);

    auto partyName = [&](const boost::uuids::uuid& id) -> QString {
        auto it = std::ranges::find_if(allParties_, [&](const auto& p) { return p.id == id; });
        return it != allParties_.end() ? QString::fromStdString(it->full_name) :
                                         QString::fromStdString(boost::uuids::to_string(id));
    };

    auto orderedAssigned = assignedParties_;
    std::ranges::sort(orderedAssigned, [&](const auto& a, const auto& b) {
        return partyName(a.party_id).compare(partyName(b.party_id), Qt::CaseInsensitive) < 0;
    });

    defaultPartyCombo_->clear();
    defaultPartyCombo_->addItem(tr("(none)"), QString());
    for (const auto& ap : orderedAssigned) {
        defaultPartyCombo_->addItem(partyName(ap.party_id),
                                    QString::fromStdString(boost::uuids::to_string(ap.party_id)));
    }

    // Always re-seed the selection from defaultPartyId_ (the authoritative
    // baseline, whether freshly set via setDefaultPartyId() or rebased via
    // rebaseDefaultParty() after a save) — not just on first populate.
    // clear() above resets the combo's currentIndex to whatever Qt
    // auto-selects for the first re-added item (index 0, "(none)"), even
    // under a QSignalBlocker, so skipping this on a later reload (e.g. the
    // load() triggered by a party add/remove alongside an unrelated
    // default-party save) would desync the visible selection from the real
    // default and, if saved again, silently clear it.
    const int idx = defaultPartyId_.is_nil() ?
        0 :
        defaultPartyCombo_->findData(
            QString::fromStdString(boost::uuids::to_string(defaultPartyId_)));
    defaultPartyCombo_->setCurrentIndex(idx >= 0 ? idx : 0);
    defaultPartyComboInitialized_ = true;
}

void AccountPartiesWidget::updateButtonStates() {
    const bool hasSelection = !assignedList_->selectedItems().isEmpty();
    const bool isConnected = clientManager_ && clientManager_->isConnected();
    const bool hasComboItem = partyCombo_->count() > 0;

    addButton_->setEnabled(!readOnly_ && isConnected && hasComboItem);
    removeButton_->setEnabled(!readOnly_ && isConnected && hasSelection);
}

}
