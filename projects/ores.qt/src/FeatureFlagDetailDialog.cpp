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
#include "ores.qt/FeatureFlagDetailDialog.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QToolBar>
#include <QIcon>
#include <QComboBox>
#include <QMdiSubWindow>
#include <QMetaObject>
#include "ui_FeatureFlagDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.variability/messaging/feature_flags_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;
using FutureResult = std::pair<bool, std::string>;

FeatureFlagDetailDialog::FeatureFlagDetailDialog(QWidget* parent)
    : DetailDialogBase(parent), ui_(new Ui::FeatureFlagDetailDialog), isDirty_(false),
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
        &FeatureFlagDetailDialog::onSaveClicked);
    toolBar_->addAction(saveAction_);

    // Create Delete action
    deleteAction_ = new QAction("Delete", this);
    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Delete, IconUtils::DefaultIconColor));
    deleteAction_->setToolTip("Delete feature flag");
    connect(deleteAction_, &QAction::triggered, this,
        &FeatureFlagDetailDialog::onDeleteClicked);
    toolBar_->addAction(deleteAction_);

    toolBar_->addSeparator();

    // Create Revert action (initially hidden)
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert feature flag to this historical version");
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

    // Version navigation actions (initially hidden)
    toolBar_->addSeparator();

    firstVersionAction_ = new QAction("First", this);
    firstVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowPrevious, IconUtils::DefaultIconColor));
    firstVersionAction_->setToolTip(tr("First version"));
    connect(firstVersionAction_, &QAction::triggered, this,
        &FeatureFlagDetailDialog::onFirstVersionClicked);
    toolBar_->addAction(firstVersionAction_);
    firstVersionAction_->setVisible(false);

    prevVersionAction_ = new QAction("Previous", this);
    prevVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowLeft, IconUtils::DefaultIconColor));
    prevVersionAction_->setToolTip(tr("Previous version"));
    connect(prevVersionAction_, &QAction::triggered, this,
        &FeatureFlagDetailDialog::onPrevVersionClicked);
    toolBar_->addAction(prevVersionAction_);
    prevVersionAction_->setVisible(false);

    nextVersionAction_ = new QAction("Next", this);
    nextVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRight, IconUtils::DefaultIconColor));
    nextVersionAction_->setToolTip(tr("Next version"));
    connect(nextVersionAction_, &QAction::triggered, this,
        &FeatureFlagDetailDialog::onNextVersionClicked);
    toolBar_->addAction(nextVersionAction_);
    nextVersionAction_->setVisible(false);

    lastVersionAction_ = new QAction("Last", this);
    lastVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowNext, IconUtils::DefaultIconColor));
    lastVersionAction_->setToolTip(tr("Last version"));
    connect(lastVersionAction_, &QAction::triggered, this,
        &FeatureFlagDetailDialog::onLastVersionClicked);
    toolBar_->addAction(lastVersionAction_);
    lastVersionAction_->setVisible(false);

    // Add toolbar to the dialog's layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);

    // Connect signals for editable fields to detect changes
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
        &FeatureFlagDetailDialog::onFieldChanged);
    connect(ui_->enabledComboBox, &QComboBox::currentIndexChanged, this,
        &FeatureFlagDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QTextEdit::textChanged, this,
        &FeatureFlagDetailDialog::onFieldChanged);

    // Initially disable save button
    updateSaveButtonState();
}

void FeatureFlagDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void FeatureFlagDetailDialog::setUsername(const std::string& username) {
    modifiedByUsername_ = username;
}

FeatureFlagDetailDialog::~FeatureFlagDetailDialog() {
    // Cancel any pending operations. The QPointer in the lambdas ensures
    // they safely handle this dialog being destroyed without blocking.
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
    }
    delete ui_;
}

void FeatureFlagDetailDialog::setFeatureFlag(
    const variability::domain::feature_flags& flag) {
    currentFlag_ = flag;
    isAddMode_ = flag.name.empty();

    setCreateMode(isAddMode_);

    ui_->nameEdit->setText(QString::fromStdString(flag.name));
    ui_->enabledComboBox->setCurrentIndex(flag.enabled ? 0 : 1);  // 0=Yes, 1=No
    ui_->descriptionEdit->setPlainText(QString::fromStdString(flag.description));
    ui_->versionEdit->setText(QString::number(flag.version));
    ui_->recordedByEdit->setText(QString::fromStdString(flag.recorded_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(flag.recorded_at));

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

void FeatureFlagDetailDialog::setCreateMode(bool createMode) {
    isAddMode_ = createMode;

    // Name is editable only in create mode
    ui_->nameEdit->setReadOnly(!createMode);

    // Metadata is not useful in create mode
    ui_->metadataGroup->setVisible(!createMode);
}

variability::domain::feature_flags FeatureFlagDetailDialog::getFeatureFlag() const {
    variability::domain::feature_flags flag = currentFlag_;
    flag.name = ui_->nameEdit->text().toStdString();
    flag.enabled = ui_->enabledComboBox->currentIndex() == 0;  // 0=Yes, 1=No
    flag.description = ui_->descriptionEdit->toPlainText().toStdString();
    flag.recorded_by = modifiedByUsername_.empty() ? "qt_user" : modifiedByUsername_;

    return flag;
}

void FeatureFlagDetailDialog::clearDialog() {
    ui_->nameEdit->clear();
    ui_->enabledComboBox->setCurrentIndex(1);  // Default to No
    ui_->descriptionEdit->clear();
    ui_->versionEdit->clear();
    ui_->recordedByEdit->clear();

    currentFlag_ = {};
    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

void FeatureFlagDetailDialog::save() {
    onSaveClicked();
}

void FeatureFlagDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    // Validate name
    if (ui_->nameEdit->text().trimmed().isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: name is empty";
        MessageBoxHelper::warning(this, "Validation Error",
            "Feature flag name is required.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for feature flag: "
                               << currentFlag_.name;

    QPointer<FeatureFlagDetailDialog> self = this;
    const variability::domain::feature_flags flagToSave = getFeatureFlag();

    QFuture<FutureResult> future =
        QtConcurrent::run([self, flagToSave]() -> FutureResult {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending save feature flag request for: "
                                       << flagToSave.name;

            variability::messaging::save_feature_flag_request request;
            request.flag = flagToSave;

            auto payload = request.serialize();
            frame request_frame(message_type::save_feature_flag_request,
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

            auto response = variability::messaging::save_feature_flag_response::
                deserialize(*payload_result);

            if (!response) {
                return {false, "Invalid server response"};
            }

            return {response->success, response->error_message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, flagToSave]() {
        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Feature flag saved successfully";

            self->isDirty_ = false;
            emit self->isDirtyChanged(false);
            self->updateSaveButtonState();

            emit self->featureFlagSaved(QString::fromStdString(flagToSave.name));
            self->notifySaveSuccess(tr("Feature flag '%1' saved")
                .arg(QString::fromStdString(flagToSave.name)));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Feature flag save failed: " << message;
            emit self->errorMessage(QString("Failed to save feature flag: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void FeatureFlagDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    if (isAddMode_) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked in add mode.";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete request for feature flag: "
                               << currentFlag_.name;

    auto reply = MessageBoxHelper::question(this, "Delete Feature Flag",
        QString("Are you sure you want to delete feature flag '%1'?")
            .arg(QString::fromStdString(currentFlag_.name)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<FeatureFlagDetailDialog> self = this;
    const std::string name = currentFlag_.name;

    QFuture<FutureResult> future =
        QtConcurrent::run([self, name]() -> FutureResult {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending delete feature flag request for: "
                                       << name;

            variability::messaging::delete_feature_flag_request request{name};
            auto payload = request.serialize();

            frame request_frame(message_type::delete_feature_flag_request,
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

            auto response = variability::messaging::delete_feature_flag_response::
                deserialize(*payload_result);

            if (!response) {
                return {false, "Invalid server response"};
            }

            return {response->success, response->error_message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, name]() {
        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "Feature flag deleted successfully.";
            emit self->statusMessage(QString("Successfully deleted feature flag: %1")
                .arg(QString::fromStdString(name)));
            emit self->featureFlagDeleted(QString::fromStdString(name));
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Feature flag deletion failed: " << message;
            emit self->errorMessage(QString("Failed to delete feature flag: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void FeatureFlagDetailDialog::onFieldChanged() {
    isDirty_ = true;
    emit isDirtyChanged(true);
    updateSaveButtonState();
}

void FeatureFlagDetailDialog::updateSaveButtonState() {
    if (saveAction_)
        saveAction_->setEnabled(isDirty_);

    if (deleteAction_)
        deleteAction_->setEnabled(!isAddMode_);
}

QString FeatureFlagDetailDialog::featureFlagName() const {
    return QString::fromStdString(currentFlag_.name);
}

void FeatureFlagDetailDialog::setReadOnly(bool readOnly, int versionNumber) {
    isReadOnly_ = readOnly;

    // Disable all editable controls in read-only mode
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->enabledComboBox->setEnabled(!readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);

    // Hide toolbar actions in read-only mode
    if (saveAction_)
        saveAction_->setVisible(!readOnly);
    if (deleteAction_)
        deleteAction_->setVisible(!readOnly);
    if (revertAction_)
        revertAction_->setVisible(readOnly);

    if (readOnly && versionNumber > 0) {
        BOOST_LOG_SEV(lg(), debug) << "Set to read-only mode, viewing version "
                                   << versionNumber;
    }
}

void FeatureFlagDetailDialog::setHistory(
    const std::vector<variability::domain::feature_flags>& history,
    int versionNumber) {
    BOOST_LOG_SEV(lg(), debug) << "Setting history with " << history.size()
                               << " versions, viewing version " << versionNumber;

    history_ = history;

    // Find the index of the requested version (history is newest-first)
    currentHistoryIndex_ = 0;
    for (int i = 0; i < static_cast<int>(history_.size()); ++i) {
        if (history_[i].version == versionNumber) {
            currentHistoryIndex_ = i;
            break;
        }
    }

    // Set read-only mode
    setReadOnly(true, versionNumber);

    // Display the current version
    displayCurrentVersion();
    showVersionNavActions(true);
}

void FeatureFlagDetailDialog::showVersionNavActions(bool visible) {
    if (firstVersionAction_)
        firstVersionAction_->setVisible(visible);
    if (prevVersionAction_)
        prevVersionAction_->setVisible(visible);
    if (nextVersionAction_)
        nextVersionAction_->setVisible(visible);
    if (lastVersionAction_)
        lastVersionAction_->setVisible(visible);
}

void FeatureFlagDetailDialog::displayCurrentVersion() {
    if (currentHistoryIndex_ < 0 ||
        currentHistoryIndex_ >= static_cast<int>(history_.size())) {
        return;
    }

    const auto& flag = history_[currentHistoryIndex_];

    // Update UI with current version data
    ui_->nameEdit->setText(QString::fromStdString(flag.name));
    ui_->enabledComboBox->setCurrentIndex(flag.enabled ? 0 : 1);
    ui_->descriptionEdit->setPlainText(QString::fromStdString(flag.description));
    ui_->versionEdit->setText(QString::number(flag.version));
    ui_->recordedByEdit->setText(QString::fromStdString(flag.recorded_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(flag.recorded_at));

    updateVersionNavButtonStates();

    // Update window title with short version format
    QWidget* parent = parentWidget();
    while (parent) {
        if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
            mdiSubWindow->setWindowTitle(QString("Feature Flag: %1 v%2")
                .arg(QString::fromStdString(flag.name))
                .arg(flag.version));
            break;
        }
        parent = parent->parentWidget();
    }

    BOOST_LOG_SEV(lg(), debug) << "Displaying version " << flag.version
                               << " (index " << currentHistoryIndex_ << ")";
}

void FeatureFlagDetailDialog::updateVersionNavButtonStates() {
    if (history_.empty()) {
        if (firstVersionAction_) firstVersionAction_->setEnabled(false);
        if (prevVersionAction_) prevVersionAction_->setEnabled(false);
        if (nextVersionAction_) nextVersionAction_->setEnabled(false);
        if (lastVersionAction_) lastVersionAction_->setEnabled(false);
        return;
    }

    // History is newest-first: index 0 = latest, index size-1 = oldest
    const bool atOldest = (currentHistoryIndex_ ==
        static_cast<int>(history_.size()) - 1);
    const bool atNewest = (currentHistoryIndex_ == 0);

    // First/Prev go to older versions (higher index)
    if (firstVersionAction_) firstVersionAction_->setEnabled(!atOldest);
    if (prevVersionAction_) prevVersionAction_->setEnabled(!atOldest);

    // Next/Last go to newer versions (lower index)
    if (nextVersionAction_) nextVersionAction_->setEnabled(!atNewest);
    if (lastVersionAction_) lastVersionAction_->setEnabled(!atNewest);
}

void FeatureFlagDetailDialog::onFirstVersionClicked() {
    // Go to oldest (highest index)
    currentHistoryIndex_ = static_cast<int>(history_.size()) - 1;
    displayCurrentVersion();
}

void FeatureFlagDetailDialog::onPrevVersionClicked() {
    // Go to older version (higher index)
    if (currentHistoryIndex_ < static_cast<int>(history_.size()) - 1) {
        currentHistoryIndex_++;
        displayCurrentVersion();
    }
}

void FeatureFlagDetailDialog::onNextVersionClicked() {
    // Go to newer version (lower index)
    if (currentHistoryIndex_ > 0) {
        currentHistoryIndex_--;
        displayCurrentVersion();
    }
}

void FeatureFlagDetailDialog::onLastVersionClicked() {
    // Go to latest (index 0)
    currentHistoryIndex_ = 0;
    displayCurrentVersion();
}

}
