/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.qt/ChangeReasonCategoryDetailDialog.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QToolBar>
#include <QIcon>
#include <QMdiSubWindow>
#include <QMetaObject>
#include <QInputDialog>
#include "ui_ChangeReasonCategoryDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;
using FutureResult = std::pair<bool, std::string>;

ChangeReasonCategoryDetailDialog::ChangeReasonCategoryDetailDialog(QWidget* parent)
    : DetailDialogBase(parent), ui_(new Ui::ChangeReasonCategoryDetailDialog), isDirty_(false),
      isAddMode_(false), isReadOnly_(false), clientManager_(nullptr),
      currentHistoryIndex_(0),
      firstVersionAction_(nullptr), prevVersionAction_(nullptr),
      nextVersionAction_(nullptr), lastVersionAction_(nullptr) {

    ui_->setupUi(this);

    // Create toolbar
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    const QColor iconColor(220, 220, 220);

    // Create Save action
    saveAction_ = new QAction("Save", this);
    saveAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_save_20_regular.svg", iconColor));
    saveAction_->setToolTip("Save changes");
    connect(saveAction_, &QAction::triggered, this,
        &ChangeReasonCategoryDetailDialog::onSaveClicked);
    toolBar_->addAction(saveAction_);

    // Create Delete action
    deleteAction_ = new QAction("Delete", this);
    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_delete_20_regular.svg", iconColor));
    deleteAction_->setToolTip("Delete category");
    connect(deleteAction_, &QAction::triggered, this,
        &ChangeReasonCategoryDetailDialog::onDeleteClicked);
    toolBar_->addAction(deleteAction_);

    toolBar_->addSeparator();

    // Create Revert action (initially hidden)
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_rotate_counterclockwise_20_regular.svg", iconColor));
    revertAction_->setToolTip("Revert category to this historical version");
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

    // Version navigation actions (initially hidden)
    toolBar_->addSeparator();

    firstVersionAction_ = new QAction("First", this);
    firstVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_previous_20_regular.svg", iconColor));
    firstVersionAction_->setToolTip(tr("First version"));
    connect(firstVersionAction_, &QAction::triggered, this,
        &ChangeReasonCategoryDetailDialog::onFirstVersionClicked);
    toolBar_->addAction(firstVersionAction_);
    firstVersionAction_->setVisible(false);

    prevVersionAction_ = new QAction("Previous", this);
    prevVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_left_20_regular.svg", iconColor));
    prevVersionAction_->setToolTip(tr("Previous version"));
    connect(prevVersionAction_, &QAction::triggered, this,
        &ChangeReasonCategoryDetailDialog::onPrevVersionClicked);
    toolBar_->addAction(prevVersionAction_);
    prevVersionAction_->setVisible(false);

    nextVersionAction_ = new QAction("Next", this);
    nextVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_right_20_regular.svg", iconColor));
    nextVersionAction_->setToolTip(tr("Next version"));
    connect(nextVersionAction_, &QAction::triggered, this,
        &ChangeReasonCategoryDetailDialog::onNextVersionClicked);
    toolBar_->addAction(nextVersionAction_);
    nextVersionAction_->setVisible(false);

    lastVersionAction_ = new QAction("Last", this);
    lastVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        ":/icons/ic_fluent_arrow_next_20_regular.svg", iconColor));
    lastVersionAction_->setToolTip(tr("Last version"));
    connect(lastVersionAction_, &QAction::triggered, this,
        &ChangeReasonCategoryDetailDialog::onLastVersionClicked);
    toolBar_->addAction(lastVersionAction_);
    lastVersionAction_->setVisible(false);

    // Add toolbar to the dialog's layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);

    // Connect signals for editable fields to detect changes
    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
        &ChangeReasonCategoryDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QTextEdit::textChanged, this,
        &ChangeReasonCategoryDetailDialog::onFieldChanged);

    // Initially disable save button
    updateSaveButtonState();
}

ChangeReasonCategoryDetailDialog::~ChangeReasonCategoryDetailDialog() {
    delete ui_;
}

void ChangeReasonCategoryDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ChangeReasonCategoryDetailDialog::setUsername(const std::string& username) {
    modifiedByUsername_ = username;
}

void ChangeReasonCategoryDetailDialog::setCategory(
    const dq::domain::change_reason_category& category) {
    currentCategory_ = category;

    ui_->codeEdit->setText(QString::fromStdString(category.code));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(category.description));

    ui_->versionEdit->setText(QString::number(category.version));
    ui_->recordedByEdit->setText(QString::fromStdString(category.recorded_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(category.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(category.change_commentary));

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

dq::domain::change_reason_category ChangeReasonCategoryDetailDialog::getCategory() const {
    dq::domain::change_reason_category category = currentCategory_;
    category.code = ui_->codeEdit->text().trimmed().toStdString();
    category.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    category.recorded_by = modifiedByUsername_;
    return category;
}

void ChangeReasonCategoryDetailDialog::setCreateMode(bool createMode) {
    isAddMode_ = createMode;

    // Code is only editable in create mode
    ui_->codeEdit->setReadOnly(!createMode);

    // Hide metadata section in create mode (shows previous version info)
    ui_->metadataGroup->setVisible(!createMode);

    // Hide delete button in create mode
    deleteAction_->setVisible(!createMode);

    if (createMode) {
        clearDialog();
    }

    updateSaveButtonState();
}

void ChangeReasonCategoryDetailDialog::setReadOnly(bool readOnly, int versionNumber) {
    isReadOnly_ = readOnly;

    ui_->codeEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);

    saveAction_->setVisible(!readOnly);
    deleteAction_->setVisible(!readOnly);
    revertAction_->setVisible(readOnly);

    if (readOnly && versionNumber > 0) {
        setWindowTitle(tr("Category Details - Version %1").arg(versionNumber));
    }
}

void ChangeReasonCategoryDetailDialog::setHistory(
    const std::vector<dq::domain::change_reason_category>& history, int versionNumber) {
    history_ = history;

    // Find the index of the requested version
    currentHistoryIndex_ = 0;
    for (size_t i = 0; i < history_.size(); ++i) {
        if (history_[i].version == versionNumber) {
            currentHistoryIndex_ = static_cast<int>(i);
            break;
        }
    }

    showVersionNavActions(history_.size() > 1);
    displayCurrentVersion();
    updateVersionNavButtonStates();
}

void ChangeReasonCategoryDetailDialog::clearDialog() {
    currentCategory_ = dq::domain::change_reason_category{};

    ui_->codeEdit->clear();
    ui_->descriptionEdit->clear();

    ui_->versionEdit->clear();
    ui_->recordedByEdit->clear();
    ui_->recordedAtEdit->clear();
    ui_->commentaryEdit->clear();

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

void ChangeReasonCategoryDetailDialog::save() {
    onSaveClicked();
}

QString ChangeReasonCategoryDetailDialog::categoryCode() const {
    return ui_->codeEdit->text();
}

void ChangeReasonCategoryDetailDialog::onFieldChanged() {
    if (!isDirty_) {
        isDirty_ = true;
        emit isDirtyChanged(true);
    }
    updateSaveButtonState();
}

void ChangeReasonCategoryDetailDialog::updateSaveButtonState() {
    bool canSave = isDirty_ && !ui_->codeEdit->text().trimmed().isEmpty();
    saveAction_->setEnabled(canSave && !isReadOnly_);
}

void ChangeReasonCategoryDetailDialog::displayCurrentVersion() {
    if (currentHistoryIndex_ >= 0 &&
        currentHistoryIndex_ < static_cast<int>(history_.size())) {
        const auto& version = history_[currentHistoryIndex_];
        setCategory(version);
        setWindowTitle(tr("Category Details - Version %1 of %2")
            .arg(version.version)
            .arg(history_.front().version));
    }
}

void ChangeReasonCategoryDetailDialog::updateVersionNavButtonStates() {
    bool hasHistory = history_.size() > 1;
    bool atFirst = currentHistoryIndex_ >= static_cast<int>(history_.size()) - 1;
    bool atLast = currentHistoryIndex_ <= 0;

    firstVersionAction_->setEnabled(hasHistory && !atFirst);
    prevVersionAction_->setEnabled(hasHistory && !atFirst);
    nextVersionAction_->setEnabled(hasHistory && !atLast);
    lastVersionAction_->setEnabled(hasHistory && !atLast);
}

void ChangeReasonCategoryDetailDialog::showVersionNavActions(bool visible) {
    firstVersionAction_->setVisible(visible);
    prevVersionAction_->setVisible(visible);
    nextVersionAction_->setVisible(visible);
    lastVersionAction_->setVisible(visible);
}

void ChangeReasonCategoryDetailDialog::onFirstVersionClicked() {
    if (!history_.empty()) {
        currentHistoryIndex_ = static_cast<int>(history_.size()) - 1;
        displayCurrentVersion();
        updateVersionNavButtonStates();
    }
}

void ChangeReasonCategoryDetailDialog::onPrevVersionClicked() {
    if (currentHistoryIndex_ < static_cast<int>(history_.size()) - 1) {
        currentHistoryIndex_++;
        displayCurrentVersion();
        updateVersionNavButtonStates();
    }
}

void ChangeReasonCategoryDetailDialog::onNextVersionClicked() {
    if (currentHistoryIndex_ > 0) {
        currentHistoryIndex_--;
        displayCurrentVersion();
        updateVersionNavButtonStates();
    }
}

void ChangeReasonCategoryDetailDialog::onLastVersionClicked() {
    if (!history_.empty()) {
        currentHistoryIndex_ = 0;
        displayCurrentVersion();
        updateVersionNavButtonStates();
    }
}

void ChangeReasonCategoryDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    // Validate code
    if (ui_->codeEdit->text().trimmed().isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: code is empty";
        MessageBoxHelper::warning(this, "Validation Error",
            "Category code is required.");
        return;
    }

    // Show commentary dialog - commentary is mandatory
    bool ok = false;
    QString commentary = QInputDialog::getMultiLineText(
        this,
        tr("Commentary Required"),
        tr("Please explain why you are making this change:"),
        QString(),
        &ok);

    if (!ok) {
        BOOST_LOG_SEV(lg(), debug) << "Save cancelled - commentary dialog rejected.";
        return;
    }

    commentary = commentary.trimmed();
    if (commentary.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: commentary is empty";
        MessageBoxHelper::warning(this, "Validation Error",
            "Commentary is required when saving changes.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for category: "
                               << currentCategory_.code;

    QPointer<ChangeReasonCategoryDetailDialog> self = this;
    dq::domain::change_reason_category categoryToSave = getCategory();
    categoryToSave.change_commentary = commentary.toStdString();

    QFuture<FutureResult> future =
        QtConcurrent::run([self, categoryToSave]() -> FutureResult {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending save category request for: "
                                       << categoryToSave.code;

            dq::messaging::save_change_reason_category_request request;
            request.category = categoryToSave;

            auto payload = request.serialize();
            frame request_frame(message_type::save_change_reason_category_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                return {false, "Failed to communicate with server"};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                return {false, "Failed to decompress server response"};
            }

            auto response = dq::messaging::save_change_reason_category_response::
                deserialize(*payload_result);

            if (!response) {
                return {false, "Invalid server response"};
            }

            return {response->success, response->message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, categoryToSave]() {
        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Category saved successfully";

            self->isDirty_ = false;
            emit self->isDirtyChanged(false);
            self->updateSaveButtonState();

            emit self->categorySaved(QString::fromStdString(categoryToSave.code));
            self->notifySaveSuccess(tr("Category '%1' saved")
                .arg(QString::fromStdString(categoryToSave.code)));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Category save failed: " << message;
            emit self->errorMessage(QString("Failed to save category: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void ChangeReasonCategoryDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    if (isAddMode_) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked in add mode.";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete request for category: "
                               << currentCategory_.code;

    auto reply = MessageBoxHelper::question(this, "Delete Category",
        QString("Are you sure you want to delete category '%1'?")
            .arg(QString::fromStdString(currentCategory_.code)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<ChangeReasonCategoryDetailDialog> self = this;
    const std::string code = currentCategory_.code;

    QFuture<FutureResult> future =
        QtConcurrent::run([self, code]() -> FutureResult {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending delete category request for: "
                                       << code;

            dq::messaging::delete_change_reason_category_request request;
            request.codes = {code};

            auto payload = request.serialize();
            frame request_frame(message_type::delete_change_reason_category_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                return {false, "Failed to communicate with server"};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                return {false, "Failed to decompress server response"};
            }

            auto response = dq::messaging::delete_change_reason_category_response::
                deserialize(*payload_result);

            if (!response) {
                return {false, "Invalid server response"};
            }

            // Check if all deletions succeeded
            if (response->results.empty()) {
                return {false, "No results in response"};
            }
            const auto& result = response->results.front();
            return {result.success, result.message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, code]() {
        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Category deleted successfully";

            emit self->statusMessage(QString("Successfully deleted category: %1")
                .arg(QString::fromStdString(code)));

            emit self->categoryDeleted(QString::fromStdString(code));
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Category delete failed: " << message;
            emit self->errorMessage(QString("Failed to delete category: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

}
