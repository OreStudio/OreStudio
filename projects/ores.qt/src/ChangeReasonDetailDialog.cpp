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
#include "ores.qt/ChangeReasonDetailDialog.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QToolBar>
#include <QIcon>
#include <QComboBox>
#include <QMdiSubWindow>
#include <QMetaObject>
#include <QInputDialog>
#include "ui_ChangeReasonDetailDialog.h"
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

ChangeReasonDetailDialog::ChangeReasonDetailDialog(QWidget* parent)
    : DetailDialogBase(parent), ui_(new Ui::ChangeReasonDetailDialog), isDirty_(false),
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
        Icon::Save, IconUtils::DefaultIconColor));
    saveAction_->setToolTip("Save changes");
    connect(saveAction_, &QAction::triggered, this,
        &ChangeReasonDetailDialog::onSaveClicked);
    toolBar_->addAction(saveAction_);

    // Create Delete action
    deleteAction_ = new QAction("Delete", this);
    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Delete, IconUtils::DefaultIconColor));
    deleteAction_->setToolTip("Delete change reason");
    connect(deleteAction_, &QAction::triggered, this,
        &ChangeReasonDetailDialog::onDeleteClicked);
    toolBar_->addAction(deleteAction_);

    toolBar_->addSeparator();

    // Create Revert action (initially hidden)
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert change reason to this historical version");
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

    // Version navigation actions (initially hidden)
    toolBar_->addSeparator();

    firstVersionAction_ = new QAction("First", this);
    firstVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowPrevious, IconUtils::DefaultIconColor));
    firstVersionAction_->setToolTip(tr("First version"));
    connect(firstVersionAction_, &QAction::triggered, this,
        &ChangeReasonDetailDialog::onFirstVersionClicked);
    toolBar_->addAction(firstVersionAction_);
    firstVersionAction_->setVisible(false);

    prevVersionAction_ = new QAction("Previous", this);
    prevVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowLeft, IconUtils::DefaultIconColor));
    prevVersionAction_->setToolTip(tr("Previous version"));
    connect(prevVersionAction_, &QAction::triggered, this,
        &ChangeReasonDetailDialog::onPrevVersionClicked);
    toolBar_->addAction(prevVersionAction_);
    prevVersionAction_->setVisible(false);

    nextVersionAction_ = new QAction("Next", this);
    nextVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRight, IconUtils::DefaultIconColor));
    nextVersionAction_->setToolTip(tr("Next version"));
    connect(nextVersionAction_, &QAction::triggered, this,
        &ChangeReasonDetailDialog::onNextVersionClicked);
    toolBar_->addAction(nextVersionAction_);
    nextVersionAction_->setVisible(false);

    lastVersionAction_ = new QAction("Last", this);
    lastVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowNext, IconUtils::DefaultIconColor));
    lastVersionAction_->setToolTip(tr("Last version"));
    connect(lastVersionAction_, &QAction::triggered, this,
        &ChangeReasonDetailDialog::onLastVersionClicked);
    toolBar_->addAction(lastVersionAction_);
    lastVersionAction_->setVisible(false);

    // Add toolbar to the dialog's layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);

    // Connect signals for editable fields to detect changes
    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
        &ChangeReasonDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
        &ChangeReasonDetailDialog::onFieldChanged);
    connect(ui_->categoryCodeComboBox, &QComboBox::currentIndexChanged, this,
        &ChangeReasonDetailDialog::onFieldChanged);
    connect(ui_->displayOrderSpinBox, &QSpinBox::valueChanged, this,
        &ChangeReasonDetailDialog::onFieldChanged);
    connect(ui_->appliesToAmendCheckBox, &QCheckBox::checkStateChanged, this,
        &ChangeReasonDetailDialog::onFieldChanged);
    connect(ui_->appliesToDeleteCheckBox, &QCheckBox::checkStateChanged, this,
        &ChangeReasonDetailDialog::onFieldChanged);
    connect(ui_->requiresCommentaryCheckBox, &QCheckBox::checkStateChanged, this,
        &ChangeReasonDetailDialog::onFieldChanged);

    // Initially disable save button
    updateSaveButtonState();
}

ChangeReasonDetailDialog::~ChangeReasonDetailDialog() {
    delete ui_;
}

void ChangeReasonDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void ChangeReasonDetailDialog::setUsername(const std::string& username) {
    modifiedByUsername_ = username;
}

void ChangeReasonDetailDialog::setCategories(
    const std::vector<dq::domain::change_reason_category>& categories) {
    categories_ = categories;
    populateCategoryComboBox();
}

void ChangeReasonDetailDialog::populateCategoryComboBox() {
    ui_->categoryCodeComboBox->clear();
    for (const auto& category : categories_) {
        ui_->categoryCodeComboBox->addItem(
            QString::fromStdString(category.code),
            QString::fromStdString(category.code));
    }
}

void ChangeReasonDetailDialog::setChangeReason(
    const dq::domain::change_reason& reason) {
    currentReason_ = reason;

    ui_->codeEdit->setText(QString::fromStdString(reason.code));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(reason.description));

    // Set category in combo box
    int index = ui_->categoryCodeComboBox->findData(
        QString::fromStdString(reason.category_code));
    if (index >= 0) {
        ui_->categoryCodeComboBox->setCurrentIndex(index);
    }

    ui_->displayOrderSpinBox->setValue(reason.display_order);
    ui_->appliesToAmendCheckBox->setChecked(reason.applies_to_amend);
    ui_->appliesToDeleteCheckBox->setChecked(reason.applies_to_delete);
    ui_->requiresCommentaryCheckBox->setChecked(reason.requires_commentary);

    ui_->versionEdit->setText(QString::number(reason.version));
    ui_->modifiedByEdit->setText(QString::fromStdString(reason.modified_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(reason.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(reason.change_commentary));

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

dq::domain::change_reason ChangeReasonDetailDialog::getChangeReason() const {
    dq::domain::change_reason reason = currentReason_;
    reason.code = ui_->codeEdit->text().trimmed().toStdString();
    reason.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    reason.category_code = ui_->categoryCodeComboBox->currentData().toString().toStdString();
    reason.display_order = ui_->displayOrderSpinBox->value();
    reason.applies_to_amend = ui_->appliesToAmendCheckBox->isChecked();
    reason.applies_to_delete = ui_->appliesToDeleteCheckBox->isChecked();
    reason.requires_commentary = ui_->requiresCommentaryCheckBox->isChecked();
    reason.modified_by = modifiedByUsername_;
    // change_commentary is set via the save dialog, not from the UI
    return reason;
}

void ChangeReasonDetailDialog::setCreateMode(bool createMode) {
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

void ChangeReasonDetailDialog::setReadOnly(bool readOnly, int versionNumber) {
    isReadOnly_ = readOnly;

    ui_->codeEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->categoryCodeComboBox->setEnabled(!readOnly);
    ui_->displayOrderSpinBox->setReadOnly(readOnly);
    ui_->appliesToAmendCheckBox->setEnabled(!readOnly);
    ui_->appliesToDeleteCheckBox->setEnabled(!readOnly);
    ui_->requiresCommentaryCheckBox->setEnabled(!readOnly);

    saveAction_->setVisible(!readOnly);
    deleteAction_->setVisible(!readOnly);
    revertAction_->setVisible(readOnly);

    if (readOnly && versionNumber > 0) {
        setWindowTitle(tr("Change Reason Details - Version %1").arg(versionNumber));
    }
}

void ChangeReasonDetailDialog::setHistory(
    const std::vector<dq::domain::change_reason>& history, int versionNumber) {
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

void ChangeReasonDetailDialog::clearDialog() {
    currentReason_ = dq::domain::change_reason{};

    ui_->codeEdit->clear();
    ui_->descriptionEdit->clear();
    if (ui_->categoryCodeComboBox->count() > 0) {
        ui_->categoryCodeComboBox->setCurrentIndex(0);
    }
    ui_->displayOrderSpinBox->setValue(0);
    ui_->appliesToAmendCheckBox->setChecked(false);
    ui_->appliesToDeleteCheckBox->setChecked(false);
    ui_->requiresCommentaryCheckBox->setChecked(false);

    ui_->versionEdit->clear();
    ui_->modifiedByEdit->clear();
    ui_->recordedAtEdit->clear();
    ui_->commentaryEdit->clear();

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

void ChangeReasonDetailDialog::save() {
    onSaveClicked();
}

QString ChangeReasonDetailDialog::changeReasonCode() const {
    return ui_->codeEdit->text();
}

void ChangeReasonDetailDialog::onFieldChanged() {
    if (!isDirty_) {
        isDirty_ = true;
        emit isDirtyChanged(true);
    }
    updateSaveButtonState();
}

void ChangeReasonDetailDialog::updateSaveButtonState() {
    bool canSave = isDirty_ && !ui_->codeEdit->text().trimmed().isEmpty() &&
                   ui_->categoryCodeComboBox->currentIndex() >= 0;
    saveAction_->setEnabled(canSave && !isReadOnly_);
}

void ChangeReasonDetailDialog::displayCurrentVersion() {
    if (currentHistoryIndex_ >= 0 &&
        currentHistoryIndex_ < static_cast<int>(history_.size())) {
        const auto& version = history_[currentHistoryIndex_];
        setChangeReason(version);
        setWindowTitle(tr("Change Reason Details - Version %1 of %2")
            .arg(version.version)
            .arg(history_.front().version));
    }
}

void ChangeReasonDetailDialog::updateVersionNavButtonStates() {
    bool hasHistory = history_.size() > 1;
    bool atFirst = currentHistoryIndex_ >= static_cast<int>(history_.size()) - 1;
    bool atLast = currentHistoryIndex_ <= 0;

    firstVersionAction_->setEnabled(hasHistory && !atFirst);
    prevVersionAction_->setEnabled(hasHistory && !atFirst);
    nextVersionAction_->setEnabled(hasHistory && !atLast);
    lastVersionAction_->setEnabled(hasHistory && !atLast);
}

void ChangeReasonDetailDialog::showVersionNavActions(bool visible) {
    firstVersionAction_->setVisible(visible);
    prevVersionAction_->setVisible(visible);
    nextVersionAction_->setVisible(visible);
    lastVersionAction_->setVisible(visible);
}

void ChangeReasonDetailDialog::onFirstVersionClicked() {
    if (!history_.empty()) {
        currentHistoryIndex_ = static_cast<int>(history_.size()) - 1;
        displayCurrentVersion();
        updateVersionNavButtonStates();
    }
}

void ChangeReasonDetailDialog::onPrevVersionClicked() {
    if (currentHistoryIndex_ < static_cast<int>(history_.size()) - 1) {
        currentHistoryIndex_++;
        displayCurrentVersion();
        updateVersionNavButtonStates();
    }
}

void ChangeReasonDetailDialog::onNextVersionClicked() {
    if (currentHistoryIndex_ > 0) {
        currentHistoryIndex_--;
        displayCurrentVersion();
        updateVersionNavButtonStates();
    }
}

void ChangeReasonDetailDialog::onLastVersionClicked() {
    if (!history_.empty()) {
        currentHistoryIndex_ = 0;
        displayCurrentVersion();
        updateVersionNavButtonStates();
    }
}

void ChangeReasonDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    // Validate code
    if (ui_->codeEdit->text().trimmed().isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: code is empty";
        MessageBoxHelper::warning(this, "Validation Error",
            "Change reason code is required.");
        return;
    }

    // Validate category
    if (ui_->categoryCodeComboBox->currentIndex() < 0) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: category not selected";
        MessageBoxHelper::warning(this, "Validation Error",
            "Category is required.");
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

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for change reason: "
                               << currentReason_.code;

    QPointer<ChangeReasonDetailDialog> self = this;
    dq::domain::change_reason reasonToSave = getChangeReason();
    reasonToSave.change_commentary = commentary.toStdString();

    QFuture<FutureResult> future =
        QtConcurrent::run([self, reasonToSave]() -> FutureResult {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending save change reason request for: "
                                       << reasonToSave.code;

            dq::messaging::save_change_reason_request request;
            request.reason = reasonToSave;

            auto payload = request.serialize();
            frame request_frame(message_type::save_change_reason_request,
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

            auto response = dq::messaging::save_change_reason_response::
                deserialize(*payload_result);

            if (!response) {
                return {false, "Invalid server response"};
            }

            return {response->success, response->message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, reasonToSave]() {
        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Change reason saved successfully";

            self->isDirty_ = false;
            emit self->isDirtyChanged(false);
            self->updateSaveButtonState();

            emit self->changeReasonSaved(QString::fromStdString(reasonToSave.code));
            self->notifySaveSuccess(tr("Change reason '%1' saved")
                .arg(QString::fromStdString(reasonToSave.code)));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Change reason save failed: " << message;
            emit self->errorMessage(QString("Failed to save change reason: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void ChangeReasonDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    if (isAddMode_) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked in add mode.";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete request for change reason: "
                               << currentReason_.code;

    auto reply = MessageBoxHelper::question(this, "Delete Change Reason",
        QString("Are you sure you want to delete change reason '%1'?")
            .arg(QString::fromStdString(currentReason_.code)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<ChangeReasonDetailDialog> self = this;
    const std::string code = currentReason_.code;

    QFuture<FutureResult> future =
        QtConcurrent::run([self, code]() -> FutureResult {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending delete change reason request for: "
                                       << code;

            dq::messaging::delete_change_reason_request request;
            request.codes = {code};

            auto payload = request.serialize();
            frame request_frame(message_type::delete_change_reason_request,
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

            auto response = dq::messaging::delete_change_reason_response::
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
            BOOST_LOG_SEV(lg(), debug) << "Change reason deleted successfully";

            emit self->statusMessage(QString("Successfully deleted change reason: %1")
                .arg(QString::fromStdString(code)));

            emit self->changeReasonDeleted(QString::fromStdString(code));
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Change reason delete failed: " << message;
            emit self->errorMessage(QString("Failed to delete change reason: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

}
