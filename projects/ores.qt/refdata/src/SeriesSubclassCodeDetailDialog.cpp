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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_qt_detail_dialog.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.qt/SeriesSubclassCodeDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/series_subclass_code_protocol.hpp"
#include "ui_SeriesSubclassCodeDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

SeriesSubclassCodeDetailDialog::SeriesSubclassCodeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::SeriesSubclassCodeDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
    // Composite child-entity tables seam: an :implements
    // 7E4A2C8D-9F1B-4E6A-8D3C-5B2A7E9F1C4D block constructs one QTableWidget
    // + QToolBar per embedded child entity (e.g. identifiers, contact
    // information), wraps each in a tab, and inserts it into this dialog's
    // tab widget. Left empty when no entity implements this kind.
}

SeriesSubclassCodeDetailDialog::~SeriesSubclassCodeDetailDialog() {
    delete ui_;
}

QTabWidget* SeriesSubclassCodeDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* SeriesSubclassCodeDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* SeriesSubclassCodeDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString SeriesSubclassCodeDetailDialog::code() const {
    return QString::fromStdString(subclass_.code);
}

void SeriesSubclassCodeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void SeriesSubclassCodeDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &SeriesSubclassCodeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &SeriesSubclassCodeDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &SeriesSubclassCodeDetailDialog::onCloseClicked);

    connect(ui_->codeEdit,
            &QLineEdit::textChanged,
            this,
            &SeriesSubclassCodeDetailDialog::onCodeChanged);
    connect(ui_->nameEdit,
            &QLineEdit::textChanged,
            this,
            &SeriesSubclassCodeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &SeriesSubclassCodeDetailDialog::onFieldChanged);
    connect(ui_->displayOrderEdit,
            &QSpinBox::valueChanged,
            this,
            &SeriesSubclassCodeDetailDialog::onFieldChanged);
}

void SeriesSubclassCodeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void SeriesSubclassCodeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void SeriesSubclassCodeDetailDialog::setCode(
    const refdata::domain::series_subclass_code& subclass) {
    subclass_ = subclass;
    updateUiFromCode();
}

void SeriesSubclassCodeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void SeriesSubclassCodeDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void SeriesSubclassCodeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void SeriesSubclassCodeDetailDialog::updateUiFromCode() {
    ui_->codeEdit->setText(QString::fromStdString(subclass_.code));
    ui_->nameEdit->setText(QString::fromStdString(subclass_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(subclass_.description));
    ui_->displayOrderEdit->setValue(subclass_.display_order);

    populateProvenance(subclass_.version,
                       subclass_.modified_by,
                       subclass_.performed_by,
                       subclass_.recorded_at,
                       subclass_.change_reason_code,
                       subclass_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void SeriesSubclassCodeDetailDialog::updateCodeFromUi() {
    if (createMode_) {
        subclass_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    subclass_.name = ui_->nameEdit->text().trimmed().toStdString();
    subclass_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    subclass_.display_order = ui_->displayOrderEdit->value();
    subclass_.modified_by = username_;
}

void SeriesSubclassCodeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void SeriesSubclassCodeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void SeriesSubclassCodeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool SeriesSubclassCodeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void SeriesSubclassCodeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save series subclass code while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }


    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    subclass_.change_reason_code = crSel->reason_code;
    subclass_.change_commentary = crSel->commentary;

    updateCodeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving series subclass code: " << subclass_.code;

    QPointer<SeriesSubclassCodeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, subclass = subclass_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_series_subclass_code_request request;
        request.data = subclass;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher,
            &QFutureWatcher<SaveResult>::finished,
            self,
            [self, watcher, crReasonCode = crSel->reason_code, crCommentary = crSel->commentary]() {
                auto result = watcher->result();
                watcher->deleteLater();

                if (result.success) {
                    BOOST_LOG_SEV(lg(), info) << "Series Subclass Code saved successfully";
                    QString code = QString::fromStdString(self->subclass_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->subclassSaved(code);
                    self->notifySaveSuccess(tr("Series Subclass Code '%1' saved").arg(code));
                } else {
                    BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
                    QString errorMsg = QString::fromStdString(result.message);
                    emit self->errorMessage(errorMsg);
                    MessageBoxHelper::critical(self, "Save Failed", errorMsg);
                }
            });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void SeriesSubclassCodeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete series subclass code while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(subclass_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Series Subclass Code",
        QString("Are you sure you want to delete series subclass code '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting series subclass code: " << subclass_.code;

    QPointer<SeriesSubclassCodeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = subclass_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_series_subclass_code_request request;
        request.codes = {code};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Series Subclass Code deleted successfully";
            emit self->statusMessage(QString("Series Subclass Code '%1' deleted").arg(code));
            emit self->subclassDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}


}
