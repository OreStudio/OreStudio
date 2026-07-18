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
#include "ores.qt/CalendarAssignmentWidget.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QListWidgetItem>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

CalendarAssignmentWidget::CalendarAssignmentWidget(QWidget* parent)
    : QWidget(parent)
    , group_(new QGroupBox("Assigned Calendars", this))
    , assignedList_(new QListWidget(this))
    , calendarCombo_(new OreCalendarComboBox(this))
    , addButton_(new QToolButton(this))
    , removeButton_(new QToolButton(this)) {

    setupUi();
    updateButtonStates();
}

void CalendarAssignmentWidget::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    auto* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    auto* groupLayout = new QVBoxLayout(group_);

    assignedList_->setAlternatingRowColors(true);
    assignedList_->setSelectionMode(QAbstractItemView::SingleSelection);
    assignedList_->setIconSize(single_flag_icon_size());
    connect(assignedList_,
            &QListWidget::itemSelectionChanged,
            this,
            &CalendarAssignmentWidget::onSelectionChanged);

    groupLayout->addWidget(assignedList_);

    auto* buttonsLayout = new QHBoxLayout();

    addButton_->setIcon(IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor));
    addButton_->setToolTip("Assign selected calendar");
    addButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(addButton_, &QToolButton::clicked, this, &CalendarAssignmentWidget::onAddClicked);

    removeButton_->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
    removeButton_->setToolTip("Revoke selected calendar");
    removeButton_->setToolButtonStyle(Qt::ToolButtonIconOnly);
    connect(removeButton_, &QToolButton::clicked, this, &CalendarAssignmentWidget::onRemoveClicked);

    buttonsLayout->addWidget(calendarCombo_);
    buttonsLayout->addWidget(addButton_);
    buttonsLayout->addWidget(removeButton_);
    buttonsLayout->addStretch();

    groupLayout->addLayout(buttonsLayout);

    mainLayout->addWidget(group_);
}

void CalendarAssignmentWidget::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CalendarAssignmentWidget::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
}

void CalendarAssignmentWidget::setLeftKey(const std::string& leftKey) {
    leftKey_ = leftKey;
}

void CalendarAssignmentWidget::setCallbacks(LoadAssignedFn loadAssigned,
                                            AssignFn assign,
                                            RevokeFn revoke) {
    loadAssignedFn_ = std::move(loadAssigned);
    assignFn_ = std::move(assign);
    revokeFn_ = std::move(revoke);
}

bool CalendarAssignmentWidget::hasPendingChanges() const {
    return !pendingAdds_.empty() || !pendingRemoves_.empty();
}

const std::vector<std::string>& CalendarAssignmentWidget::pendingAdds() const {
    return pendingAdds_;
}

const std::vector<std::string>& CalendarAssignmentWidget::pendingRemoves() const {
    return pendingRemoves_;
}

void CalendarAssignmentWidget::load() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot load calendars: not connected";
        return;
    }

    const bool has_left_key = !leftKey_.empty();
    if (has_left_key) {
        BOOST_LOG_SEV(lg(), debug) << "Loading calendars for: " << leftKey_;
    } else {
        BOOST_LOG_SEV(lg(), debug) << "Loading available calendars for new entity";
    }

    QPointer<CalendarAssignmentWidget> self = this;
    const auto leftKey = leftKey_;

    struct LoadFutureResult {
        bool success;
        std::vector<std::string> assignedCodes;
        std::vector<std::string> allCodes;
    };

    auto* watcher = new QFutureWatcher<LoadFutureResult>(this);
    connect(watcher, &QFutureWatcher<LoadFutureResult>::finished, this, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        if (result.success) {
            self->assignedCodes_ = std::move(result.assignedCodes);
            self->allCalendarCodes_ = std::move(result.allCodes);
            self->pendingAdds_.clear();
            self->pendingRemoves_.clear();
            self->refreshView();
            emit self->dataLoaded();
            BOOST_LOG_SEV(lg(), debug) << "Loaded " << self->assignedCodes_.size()
                                       << " assigned, " << self->allCalendarCodes_.size()
                                       << " total calendars";
        } else {
            emit self->errorMessage("Load Failed", "Failed to load calendars");
        }
    });

    auto* clientManager = clientManager_;
    auto loadAssignedFn = loadAssignedFn_;
    QFuture<LoadFutureResult> future = QtConcurrent::run(
        [self, clientManager, leftKey, has_left_key, loadAssignedFn]() -> LoadFutureResult {
            if (!self)
                return {.success = false};

            auto allCodes = fetch_calendar_codes(clientManager);

            if (has_left_key && loadAssignedFn) {
                auto assignedResult = loadAssignedFn(clientManager, leftKey);
                if (!assignedResult.success) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Failed to fetch assigned calendars: " << assignedResult.message.toStdString();
                    return {.success = false};
                }
                return {.success = true,
                        .assignedCodes = std::move(assignedResult.calendarCodes),
                        .allCodes = std::move(allCodes)};
            }

            return {.success = true, .assignedCodes = {}, .allCodes = std::move(allCodes)};
        });

    watcher->setFuture(future);
}

void CalendarAssignmentWidget::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    updateButtonStates();
}

void CalendarAssignmentWidget::onAddClicked() {
    if (calendarCombo_->count() == 0)
        return;

    const auto code = calendarCombo_->currentText().toStdString();
    if (code.empty())
        return;

    auto it = std::find(pendingRemoves_.begin(), pendingRemoves_.end(), code);
    if (it != pendingRemoves_.end()) {
        pendingRemoves_.erase(it);
    } else {
        pendingAdds_.push_back(code);
    }

    refreshView();
    updateButtonStates();
    emit assignmentsChanged();
}

void CalendarAssignmentWidget::onRemoveClicked() {
    auto selected = assignedList_->selectedItems();
    if (selected.isEmpty())
        return;

    const auto code = selected.first()->data(Qt::UserRole).toString().toStdString();
    if (code.empty())
        return;

    auto it = std::find(pendingAdds_.begin(), pendingAdds_.end(), code);
    if (it != pendingAdds_.end()) {
        pendingAdds_.erase(it);
    } else {
        pendingRemoves_.push_back(code);
    }

    refreshView();
    updateButtonStates();
    emit assignmentsChanged();
}

void CalendarAssignmentWidget::onSelectionChanged() {
    updateButtonStates();
}

void CalendarAssignmentWidget::refreshView() {
    assignedList_->clear();

    auto isRemoved = [&](const std::string& code) {
        return std::find(pendingRemoves_.begin(), pendingRemoves_.end(), code) !=
               pendingRemoves_.end();
    };

    auto flagIcon = [&](const std::string& code) -> QIcon {
        if (!imageCache_)
            return {};
        return imageCache_->getCalendarFlagIcon(code);
    };

    std::vector<std::string> effectiveCodes;

    for (const auto& code : assignedCodes_) {
        if (!isRemoved(code)) {
            effectiveCodes.push_back(code);
            auto* item = new QListWidgetItem(flagIcon(code), QString::fromStdString(code));
            item->setData(Qt::UserRole, QString::fromStdString(code));
            assignedList_->addItem(item);
        }
    }

    for (const auto& code : pendingAdds_) {
        effectiveCodes.push_back(code);
        auto* item = new QListWidgetItem(flagIcon(code), QString::fromStdString(code));
        item->setData(Qt::UserRole, QString::fromStdString(code));
        assignedList_->addItem(item);
    }

    calendarCombo_->clear();
    std::vector<std::string> sortedAll = allCalendarCodes_;
    std::sort(sortedAll.begin(), sortedAll.end());
    for (const auto& code : sortedAll) {
        bool inEffective =
            std::find(effectiveCodes.begin(), effectiveCodes.end(), code) != effectiveCodes.end();
        if (!inEffective)
            calendarCombo_->addItem(QString::fromStdString(code));
    }
    if (imageCache_)
        apply_flag_icons(calendarCombo_, imageCache_, FlagSource::Calendar);

    group_->setTitle(
        QString("Assigned Calendars (%1)").arg(static_cast<int>(effectiveCodes.size())));

    updateButtonStates();
}

void CalendarAssignmentWidget::updateButtonStates() {
    const bool hasSelection = !assignedList_->selectedItems().isEmpty();
    const bool isConnected = clientManager_ && clientManager_->isConnected();
    const bool hasComboItem = calendarCombo_->count() > 0;

    addButton_->setEnabled(!readOnly_ && isConnected && hasComboItem);
    removeButton_->setEnabled(!readOnly_ && isConnected && hasSelection);
}

void CalendarAssignmentWidget::commitChanges(
    const std::string& changeReasonCode,
    const std::string& changeCommentary,
    std::function<void(bool success, const QString& message)> onComplete) {

    if (!clientManager_ || !assignFn_ || !revokeFn_ || leftKey_.empty()) {
        if (onComplete)
            onComplete(false, "Calendar assignment widget not fully configured");
        return;
    }

    QPointer<CalendarAssignmentWidget> self = this;
    auto* clientManager = clientManager_;
    const auto leftKey = leftKey_;
    const auto adds = pendingAdds_;
    const auto removes = pendingRemoves_;
    auto assignFn = assignFn_;
    auto revokeFn = revokeFn_;

    struct CommitResult {
        bool success;
        QString message;
    };

    auto* watcher = new QFutureWatcher<CommitResult>(this);
    connect(watcher,
            &QFutureWatcher<CommitResult>::finished,
            this,
            [self, watcher, onComplete]() {
                auto result = watcher->result();
                watcher->deleteLater();
                if (self && result.success) {
                    self->load();
                }
                if (onComplete)
                    onComplete(result.success, result.message);
            });

    QFuture<CommitResult> future = QtConcurrent::run(
        [clientManager, leftKey, adds, removes, assignFn, revokeFn, changeReasonCode,
         changeCommentary]() -> CommitResult {
            for (const auto& code : adds) {
                auto result =
                    assignFn(clientManager, leftKey, code, changeReasonCode, changeCommentary);
                if (!result.success)
                    return {.success = false, .message = result.message};
            }
            for (const auto& code : removes) {
                auto result = revokeFn(clientManager, leftKey, code);
                if (!result.success)
                    return {.success = false, .message = result.message};
            }
            return {.success = true, .message = {}};
        });

    watcher->setFuture(future);
}

}
