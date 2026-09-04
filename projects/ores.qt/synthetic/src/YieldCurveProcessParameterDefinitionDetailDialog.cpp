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
#include "ores.qt/YieldCurveProcessParameterDefinitionDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/yield_curve_process_parameter_definition_protocol.hpp"
#include "ui_YieldCurveProcessParameterDefinitionDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

YieldCurveProcessParameterDefinitionDetailDialog::YieldCurveProcessParameterDefinitionDetailDialog(
    QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::YieldCurveProcessParameterDefinitionDetailDialog)
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

YieldCurveProcessParameterDefinitionDetailDialog::
    ~YieldCurveProcessParameterDefinitionDetailDialog() {
    delete ui_;
}

QTabWidget* YieldCurveProcessParameterDefinitionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* YieldCurveProcessParameterDefinitionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* YieldCurveProcessParameterDefinitionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString YieldCurveProcessParameterDefinitionDetailDialog::code() const {
    return QString::fromStdString(parameter_definition_.parameter_name);
}

void YieldCurveProcessParameterDefinitionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void YieldCurveProcessParameterDefinitionDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onCloseClicked);

    connect(ui_->processTypeCodeEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
    connect(ui_->parameterNameEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onCodeChanged);
    connect(ui_->displayNameEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
    connect(ui_->symbolEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
    connect(ui_->shortLabelEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
    connect(ui_->dataTypeEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
    connect(ui_->defaultValueEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
    connect(ui_->minValueEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
    connect(ui_->maxValueEdit,
            &QLineEdit::textChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
    connect(ui_->displayOrderEdit,
            &QSpinBox::valueChanged,
            this,
            &YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged);
}

void YieldCurveProcessParameterDefinitionDetailDialog::setClientManager(
    ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void YieldCurveProcessParameterDefinitionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void YieldCurveProcessParameterDefinitionDetailDialog::setDefinition(
    const synthetic::domain::yield_curve_process_parameter_definition& parameter_definition) {
    parameter_definition_ = parameter_definition;
    updateUiFromDefinition();
}

void YieldCurveProcessParameterDefinitionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->parameterNameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        parameter_definition_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void YieldCurveProcessParameterDefinitionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void YieldCurveProcessParameterDefinitionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->processTypeCodeEdit->setReadOnly(readOnly);
    ui_->parameterNameEdit->setReadOnly(true);
    ui_->displayNameEdit->setReadOnly(readOnly);
    ui_->symbolEdit->setReadOnly(readOnly);
    ui_->shortLabelEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->dataTypeEdit->setReadOnly(readOnly);
    ui_->defaultValueEdit->setReadOnly(readOnly);
    ui_->minValueEdit->setReadOnly(readOnly);
    ui_->maxValueEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void YieldCurveProcessParameterDefinitionDetailDialog::updateUiFromDefinition() {
    ui_->processTypeCodeEdit->setText(
        QString::fromStdString(parameter_definition_.process_type_code));
    ui_->parameterNameEdit->setText(QString::fromStdString(parameter_definition_.parameter_name));
    ui_->displayNameEdit->setText(QString::fromStdString(parameter_definition_.display_name));
    ui_->symbolEdit->setText(parameter_definition_.symbol ?
                                 QString::fromStdString(*parameter_definition_.symbol) :
                                 QString{});
    ui_->shortLabelEdit->setText(QString::fromStdString(parameter_definition_.short_label));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(parameter_definition_.description));
    ui_->dataTypeEdit->setText(QString::fromStdString(parameter_definition_.data_type));
    ui_->defaultValueEdit->setText(QString::number(parameter_definition_.default_value));
    ui_->minValueEdit->setText(parameter_definition_.min_value ?
                                   QString::number(*parameter_definition_.min_value) :
                                   QString{});
    ui_->maxValueEdit->setText(parameter_definition_.max_value ?
                                   QString::number(*parameter_definition_.max_value) :
                                   QString{});
    ui_->displayOrderEdit->setValue(parameter_definition_.display_order);

    populateProvenance(parameter_definition_.version,
                       parameter_definition_.modified_by,
                       parameter_definition_.performed_by,
                       parameter_definition_.recorded_at,
                       parameter_definition_.change_reason_code,
                       parameter_definition_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void YieldCurveProcessParameterDefinitionDetailDialog::updateDefinitionFromUi() {
    parameter_definition_.process_type_code =
        ui_->processTypeCodeEdit->text().trimmed().toStdString();
    if (createMode_) {
        parameter_definition_.parameter_name =
            ui_->parameterNameEdit->text().trimmed().toStdString();
    }
    parameter_definition_.display_name = ui_->displayNameEdit->text().trimmed().toStdString();
    {
        const auto symbol_str = ui_->symbolEdit->text().trimmed().toStdString();
        parameter_definition_.symbol =
            symbol_str.empty() ? std::nullopt : std::optional<std::string>(symbol_str);
    }
    parameter_definition_.short_label = ui_->shortLabelEdit->text().trimmed().toStdString();
    parameter_definition_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    parameter_definition_.data_type = ui_->dataTypeEdit->text().trimmed().toStdString();
    parameter_definition_.default_value = ui_->defaultValueEdit->text().trimmed().toDouble();
    parameter_definition_.min_value =
        ui_->minValueEdit->text().trimmed().isEmpty() ?
            std::nullopt :
            std::optional<double>(ui_->minValueEdit->text().trimmed().toDouble());
    parameter_definition_.max_value =
        ui_->maxValueEdit->text().trimmed().isEmpty() ?
            std::nullopt :
            std::optional<double>(ui_->maxValueEdit->text().trimmed().toDouble());
    parameter_definition_.display_order = ui_->displayOrderEdit->value();
    parameter_definition_.modified_by = username_;
}

void YieldCurveProcessParameterDefinitionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void YieldCurveProcessParameterDefinitionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void YieldCurveProcessParameterDefinitionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool YieldCurveProcessParameterDefinitionDetailDialog::validateInput() {
    const QString process_type_code_val = ui_->processTypeCodeEdit->text().trimmed();
    const QString parameter_name_val = ui_->parameterNameEdit->text().trimmed();
    const QString display_name_val = ui_->displayNameEdit->text().trimmed();
    const QString short_label_val = ui_->shortLabelEdit->text().trimmed();
    const QString data_type_val = ui_->dataTypeEdit->text().trimmed();

    return true && !process_type_code_val.isEmpty() && !parameter_name_val.isEmpty() &&
           !display_name_val.isEmpty() && !short_label_val.isEmpty() && !data_type_val.isEmpty();
}

void YieldCurveProcessParameterDefinitionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save yield curve process parameter definition while disconnected from server.");
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
    parameter_definition_.change_reason_code = crSel->reason_code;
    parameter_definition_.change_commentary = crSel->commentary;

    updateDefinitionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving yield curve process parameter definition: "
                              << parameter_definition_.parameter_name;

    QPointer<YieldCurveProcessParameterDefinitionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, parameter_definition = parameter_definition_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::save_yield_curve_process_parameter_definition_request request;
        request.data = parameter_definition;
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
                    BOOST_LOG_SEV(lg(), info)
                        << "Yield Curve Process Parameter Definition saved successfully";
                    QString code =
                        QString::fromStdString(self->parameter_definition_.parameter_name);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->parameter_definitionSaved(code);
                    self->notifySaveSuccess(
                        tr("Yield Curve Process Parameter Definition '%1' saved").arg(code));
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

void YieldCurveProcessParameterDefinitionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this,
                                  "Disconnected",
                                  "Cannot delete yield curve process parameter definition while "
                                  "disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(parameter_definition_.parameter_name);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Yield Curve Process Parameter Definition",
        QString("Are you sure you want to delete yield curve process parameter definition '%1'?")
            .arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting yield curve process parameter definition: "
                              << parameter_definition_.parameter_name;

    QPointer<YieldCurveProcessParameterDefinitionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self,
                 id_str = boost::uuids::to_string(parameter_definition_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::delete_yield_curve_process_parameter_definition_request request;
        request.ids = {id_str};
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
            BOOST_LOG_SEV(lg(), info)
                << "Yield Curve Process Parameter Definition deleted successfully";
            emit self->statusMessage(
                QString("Yield Curve Process Parameter Definition '%1' deleted").arg(code));
            emit self->parameter_definitionDeleted(code);
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
