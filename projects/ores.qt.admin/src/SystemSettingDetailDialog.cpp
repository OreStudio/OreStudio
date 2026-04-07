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
#include "ores.qt/SystemSettingDetailDialog.hpp"

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QToolBar>
#include <QIcon>
#include <QComboBox>
#include <QIntValidator>
#include <QJsonDocument>
#include <QMdiSubWindow>
#include <QMetaObject>
#include "ui_SystemSettingDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;
using FutureResult = std::pair<bool, std::string>;

SystemSettingDetailDialog::SystemSettingDetailDialog(QWidget* parent)
    : DetailDialogBase(parent), ui_(new Ui::SystemSettingDetailDialog),
      intValidator_(new QIntValidator(this)),
      isDirty_(false),
      isAddMode_(false), isReadOnly_(false), clientManager_(nullptr),
      currentHistoryIndex_(0),
      firstVersionAction_(nullptr), prevVersionAction_(nullptr),
      nextVersionAction_(nullptr), lastVersionAction_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);

    // Create toolbar (for historical navigation only, hidden by default)
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    // Create Revert action
    revertAction_ = new QAction("Revert", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRotateCounterclockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert system setting to this historical version");
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

    // Version navigation actions (initially hidden)
    toolBar_->addSeparator();

    firstVersionAction_ = new QAction("First", this);
    firstVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowPrevious, IconUtils::DefaultIconColor));
    firstVersionAction_->setToolTip(tr("First version"));
    connect(firstVersionAction_, &QAction::triggered, this,
        &SystemSettingDetailDialog::onFirstVersionClicked);
    toolBar_->addAction(firstVersionAction_);
    firstVersionAction_->setVisible(false);

    prevVersionAction_ = new QAction("Previous", this);
    prevVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowLeft, IconUtils::DefaultIconColor));
    prevVersionAction_->setToolTip(tr("Previous version"));
    connect(prevVersionAction_, &QAction::triggered, this,
        &SystemSettingDetailDialog::onPrevVersionClicked);
    toolBar_->addAction(prevVersionAction_);
    prevVersionAction_->setVisible(false);

    nextVersionAction_ = new QAction("Next", this);
    nextVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowRight, IconUtils::DefaultIconColor));
    nextVersionAction_->setToolTip(tr("Next version"));
    connect(nextVersionAction_, &QAction::triggered, this,
        &SystemSettingDetailDialog::onNextVersionClicked);
    toolBar_->addAction(nextVersionAction_);
    nextVersionAction_->setVisible(false);

    lastVersionAction_ = new QAction("Last", this);
    lastVersionAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowNext, IconUtils::DefaultIconColor));
    lastVersionAction_->setToolTip(tr("Last version"));
    connect(lastVersionAction_, &QAction::triggered, this,
        &SystemSettingDetailDialog::onLastVersionClicked);
    toolBar_->addAction(lastVersionAction_);
    lastVersionAction_->setVisible(false);

    // Add toolbar to the dialog's layout (hidden by default, shown in historical mode)
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout) {
        mainLayout->insertWidget(0, toolBar_);
        toolBar_->setVisible(false);
    }

    // Setup bottom buttons
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);
    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
    connect(ui_->saveButton, &QPushButton::clicked, this,
        &SystemSettingDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
        &SystemSettingDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
        &SystemSettingDetailDialog::onCloseClicked);

    // Connect data type selector
    connect(ui_->dataTypeComboBox, &QComboBox::currentTextChanged, this,
        [this](const QString&) { onDataTypeChanged(); });

    // Connect signals for editable fields to detect changes
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
        &SystemSettingDetailDialog::onFieldChanged);
    connect(ui_->dataTypeComboBox, &QComboBox::currentIndexChanged, this,
        [this](int) { onDataTypeChanged(); });
    connect(ui_->valueBoolCombo, &QComboBox::currentIndexChanged, this,
        &SystemSettingDetailDialog::onFieldChanged);
    connect(ui_->valueLineEdit, &QLineEdit::textChanged, this,
        &SystemSettingDetailDialog::onFieldChanged);
    connect(ui_->valueJsonEdit, &QPlainTextEdit::textChanged, this,
        &SystemSettingDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
        &SystemSettingDetailDialog::onFieldChanged);

    // Initially disable save button
    updateSaveButtonState();
}

void SystemSettingDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void SystemSettingDetailDialog::setUsername(const std::string& username) {
    modifiedByUsername_ = username;
}

SystemSettingDetailDialog::~SystemSettingDetailDialog() {
    // Cancel any pending operations. The QPointer in the lambdas ensures
    // they safely handle this dialog being destroyed without blocking.
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
    }
    delete ui_;
}

QTabWidget* SystemSettingDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* SystemSettingDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* SystemSettingDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void SystemSettingDetailDialog::setSystemSetting(
    const variability::domain::system_setting& flag) {
    currentFlag_ = flag;
    isAddMode_ = flag.name.empty();

    setCreateMode(isAddMode_);

    ui_->nameEdit->setText(QString::fromStdString(flag.name));
    populateValueWidgets(flag.data_type, flag.value);
    ui_->descriptionEdit->setPlainText(QString::fromStdString(flag.description));
    populateProvenance(currentFlag_.version, currentFlag_.modified_by,
                       currentFlag_.performed_by, currentFlag_.recorded_at,
                       currentFlag_.change_reason_code, currentFlag_.change_commentary);

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

void SystemSettingDetailDialog::setCreateMode(bool createMode) {
    isAddMode_ = createMode;

    // Name is editable only in create mode
    ui_->nameEdit->setReadOnly(!createMode);

    setProvenanceEnabled(!createMode);
}

variability::domain::system_setting SystemSettingDetailDialog::getSystemSetting() const {
    variability::domain::system_setting flag = currentFlag_;
    flag.name = ui_->nameEdit->text().toStdString();
    flag.data_type = ui_->dataTypeComboBox->currentText().toStdString();
    flag.value = currentValueText().toStdString();
    flag.description = ui_->descriptionEdit->toPlainText().toStdString();
    flag.modified_by = modifiedByUsername_.empty() ? "qt_user" : modifiedByUsername_;

    return flag;
}

QString SystemSettingDetailDialog::currentValueText() const {
    const int page = ui_->valueStack->currentIndex();
    if (page == 0)
        return ui_->valueBoolCombo->currentText();
    if (page == 2)
        return ui_->valueJsonEdit->toPlainText().trimmed();
    return ui_->valueLineEdit->text().trimmed();
}

void SystemSettingDetailDialog::populateValueWidgets(
    const std::string& dataType, const std::string& value) {

    // Block signals so populateValueWidgets doesn't trigger onFieldChanged
    QSignalBlocker b1(ui_->dataTypeComboBox);
    QSignalBlocker b2(ui_->valueBoolCombo);
    QSignalBlocker b3(ui_->valueLineEdit);
    QSignalBlocker b4(ui_->valueJsonEdit);

    const QString dt = QString::fromStdString(dataType);
    const QString val = QString::fromStdString(value);

    // Set the data type combo
    const int idx = ui_->dataTypeComboBox->findText(dt);
    ui_->dataTypeComboBox->setCurrentIndex(idx >= 0 ? idx : 0);

    // Switch the value stack and populate
    switchValuePage(dt);

    if (dt == "boolean") {
        ui_->valueBoolCombo->setCurrentIndex(val == "true" ? 0 : 1);
    } else if (dt == "json") {
        ui_->valueJsonEdit->setPlainText(val);
    } else {
        ui_->valueLineEdit->setText(val);
    }
}

void SystemSettingDetailDialog::switchValuePage(const QString& dataType) {
    if (dataType == "boolean") {
        ui_->valueStack->setCurrentIndex(0);
        ui_->valueLineEdit->setValidator(nullptr);
        ui_->valueHintLabel->setText(tr("Accepted values: true, false"));
    } else if (dataType == "integer") {
        ui_->valueStack->setCurrentIndex(1);
        ui_->valueLineEdit->setValidator(intValidator_);
        ui_->valueHintLabel->setText(tr("Must be a whole number"));
    } else if (dataType == "string") {
        ui_->valueStack->setCurrentIndex(1);
        ui_->valueLineEdit->setValidator(nullptr);
        ui_->valueHintLabel->setText(tr("Any text value"));
    } else if (dataType == "json") {
        ui_->valueStack->setCurrentIndex(2);
        ui_->valueLineEdit->setValidator(nullptr);
        ui_->valueHintLabel->setText(tr("Must be valid JSON"));
    }
}

void SystemSettingDetailDialog::clearDialog() {
    ui_->nameEdit->clear();
    populateValueWidgets("boolean", "false");
    ui_->descriptionEdit->clear();
    clearProvenance();

    currentFlag_ = {};
    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

void SystemSettingDetailDialog::onDataTypeChanged() {
    switchValuePage(ui_->dataTypeComboBox->currentText());
    onFieldChanged();
}

void SystemSettingDetailDialog::save() {
    onSaveClicked();
}

void SystemSettingDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    // Validate name
    if (ui_->nameEdit->text().trimmed().isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: name is empty";
        MessageBoxHelper::warning(this, "Validation Error",
            "System setting name is required.");
        return;
    }

    // Validate value against data type
    const QString dataType = ui_->dataTypeComboBox->currentText();
    const QString value = currentValueText();

    if (dataType == "integer") {
        bool ok = false;
        value.toInt(&ok);
        if (!ok) {
            BOOST_LOG_SEV(lg(), warn) << "Validation failed: value is not an integer";
            MessageBoxHelper::warning(this, "Validation Error",
                QString("Value '%1' is not a valid integer.").arg(value));
            return;
        }
    } else if (dataType == "json") {
        if (!value.isEmpty()) {
            QJsonParseError err;
            QJsonDocument::fromJson(value.toUtf8(), &err);
            if (err.error != QJsonParseError::NoError) {
                BOOST_LOG_SEV(lg(), warn) << "Validation failed: value is not valid JSON";
                MessageBoxHelper::warning(this, "Validation Error",
                    QString("Value is not valid JSON: %1").arg(err.errorString()));
                return;
            }
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for system setting: "
                               << currentFlag_.name;

    const auto crOpType = isAddMode_
        ? ChangeReasonDialog::OperationType::Create
        : ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, isDirty_,
        isAddMode_ ? "system" : "common");
    if (!crSel) return;

    QPointer<SystemSettingDetailDialog> self = this;
    variability::domain::system_setting flagToSave = getSystemSetting();
    flagToSave.change_reason_code = crSel->reason_code;
    flagToSave.change_commentary = crSel->commentary;

    QFuture<FutureResult> future =
        QtConcurrent::run([self, flagToSave]() -> FutureResult {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending save system setting request for: "
                                       << flagToSave.name;

            variability::messaging::save_setting_request request;
            request.data = flagToSave;

            auto response_result =
                self->clientManager_->process_authenticated_request(std::move(request));

            if (!response_result) {
                return {false, "Failed to communicate with server"};
            }


            return {response_result->success, response_result->message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, flagToSave]() {
        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "System setting saved successfully";

            self->isDirty_ = false;
            emit self->isDirtyChanged(false);
            self->updateSaveButtonState();

            emit self->systemSettingSaved(QString::fromStdString(flagToSave.name));
            self->notifySaveSuccess(tr("System setting '%1' saved")
                .arg(QString::fromStdString(flagToSave.name)));
        } else {
            BOOST_LOG_SEV(lg(), error) << "System setting save failed: " << message;
            emit self->errorMessage(QString("Failed to save system setting: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void SystemSettingDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    if (isAddMode_) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked in add mode.";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete request for system setting: "
                               << currentFlag_.name;

    auto reply = MessageBoxHelper::question(this, "Delete System Setting",
        QString("Are you sure you want to delete system setting '%1'?")
            .arg(QString::fromStdString(currentFlag_.name)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    {
        const auto crSel = promptChangeReason(
            ChangeReasonDialog::OperationType::Delete, true, "common");
        if (!crSel) return;
    }

    QPointer<SystemSettingDetailDialog> self = this;
    const std::string name = currentFlag_.name;

    QFuture<FutureResult> future =
        QtConcurrent::run([self, name]() -> FutureResult {
            if (!self) return {false, ""};

            BOOST_LOG_SEV(lg(), debug) << "Sending delete system setting request for: "
                                       << name;

            variability::messaging::delete_setting_request request{name};
            auto response_result =
self->clientManager_->process_authenticated_request(std::move(request));

            if (!response_result) {
                return {false, "Failed to communicate with server"};
            }


            return {response_result->success, response_result->error_message};
        });

    auto* watcher = new QFutureWatcher<FutureResult>(self);
    connect(watcher, &QFutureWatcher<FutureResult>::finished, self,
        [self, watcher, name]() {
        if (!self) return;

        auto [success, message] = watcher->result();
        watcher->deleteLater();

        if (success) {
            BOOST_LOG_SEV(lg(), debug) << "System setting deleted successfully.";
            emit self->statusMessage(QString("Successfully deleted system setting: %1")
                .arg(QString::fromStdString(name)));
            emit self->systemSettingDeleted(QString::fromStdString(name));
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "System setting deletion failed: " << message;
            emit self->errorMessage(QString("Failed to delete system setting: %1")
                .arg(QString::fromStdString(message)));
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(message));
        }
    });

    watcher->setFuture(future);
}

void SystemSettingDetailDialog::onFieldChanged() {
    isDirty_ = true;
    emit isDirtyChanged(true);
    updateSaveButtonState();
}

void SystemSettingDetailDialog::updateSaveButtonState() {
    ui_->saveButton->setEnabled(isDirty_);
    ui_->deleteButton->setEnabled(!isAddMode_);
}

QString SystemSettingDetailDialog::systemSettingName() const {
    return QString::fromStdString(currentFlag_.name);
}

void SystemSettingDetailDialog::setReadOnly(bool readOnly, int versionNumber) {
    isReadOnly_ = readOnly;

    // Disable all editable controls in read-only mode
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->dataTypeComboBox->setEnabled(!readOnly);
    ui_->valueBoolCombo->setEnabled(!readOnly);
    ui_->valueLineEdit->setReadOnly(readOnly);
    ui_->valueJsonEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);

    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
    toolBar_->setVisible(readOnly);

    if (readOnly && versionNumber > 0) {
        BOOST_LOG_SEV(lg(), debug) << "Set to read-only mode, viewing version "
                                   << versionNumber;
    }
}

void SystemSettingDetailDialog::setHistory(
    const std::vector<variability::domain::system_setting>& history,
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

void SystemSettingDetailDialog::showVersionNavActions(bool visible) {
    if (firstVersionAction_)
        firstVersionAction_->setVisible(visible);
    if (prevVersionAction_)
        prevVersionAction_->setVisible(visible);
    if (nextVersionAction_)
        nextVersionAction_->setVisible(visible);
    if (lastVersionAction_)
        lastVersionAction_->setVisible(visible);
}

void SystemSettingDetailDialog::displayCurrentVersion() {
    if (currentHistoryIndex_ < 0 ||
        currentHistoryIndex_ >= static_cast<int>(history_.size())) {
        return;
    }

    const auto& flag = history_[currentHistoryIndex_];

    // Update UI with current version data
    ui_->nameEdit->setText(QString::fromStdString(flag.name));
    populateValueWidgets(flag.data_type, flag.value);
    ui_->descriptionEdit->setPlainText(QString::fromStdString(flag.description));
    populateProvenance(flag.version, flag.modified_by, flag.performed_by,
                       flag.recorded_at, flag.change_reason_code, flag.change_commentary);

    updateVersionNavButtonStates();

    // Update window title with short version format
    QWidget* parent = parentWidget();
    while (parent) {
        if (auto* mdiSubWindow = qobject_cast<QMdiSubWindow*>(parent)) {
            mdiSubWindow->setWindowTitle(QString("System Setting: %1 v%2")
                .arg(QString::fromStdString(flag.name))
                .arg(flag.version));
            break;
        }
        parent = parent->parentWidget();
    }

    BOOST_LOG_SEV(lg(), debug) << "Displaying version " << flag.version
                               << " (index " << currentHistoryIndex_ << ")";
}

void SystemSettingDetailDialog::updateVersionNavButtonStates() {
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

void SystemSettingDetailDialog::onFirstVersionClicked() {
    // Go to oldest (highest index)
    currentHistoryIndex_ = static_cast<int>(history_.size()) - 1;
    displayCurrentVersion();
}

void SystemSettingDetailDialog::onPrevVersionClicked() {
    // Go to older version (higher index)
    if (currentHistoryIndex_ < static_cast<int>(history_.size()) - 1) {
        currentHistoryIndex_++;
        displayCurrentVersion();
    }
}

void SystemSettingDetailDialog::onNextVersionClicked() {
    // Go to newer version (lower index)
    if (currentHistoryIndex_ > 0) {
        currentHistoryIndex_--;
        displayCurrentVersion();
    }
}

void SystemSettingDetailDialog::onLastVersionClicked() {
    // Go to latest (index 0)
    currentHistoryIndex_ = 0;
    displayCurrentVersion();
}

}
