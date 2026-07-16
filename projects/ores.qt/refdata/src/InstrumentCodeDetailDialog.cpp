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
#include "ores.qt/InstrumentCodeDetailDialog.hpp"
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/instrument_code_protocol.hpp"
#include "ui_InstrumentCodeDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

InstrumentCodeDetailDialog::InstrumentCodeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::InstrumentCodeDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
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

InstrumentCodeDetailDialog::~InstrumentCodeDetailDialog() {
    delete ui_;
}

QTabWidget* InstrumentCodeDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* InstrumentCodeDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* InstrumentCodeDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString InstrumentCodeDetailDialog::code() const {
    return QString::fromStdString(code__.code);
}

void InstrumentCodeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void InstrumentCodeDetailDialog::setupCombos() {}

void InstrumentCodeDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &InstrumentCodeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &InstrumentCodeDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &InstrumentCodeDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &InstrumentCodeDetailDialog::onCodeChanged);
    connect(
        ui_->nameEdit, &QLineEdit::textChanged, this, &InstrumentCodeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &InstrumentCodeDetailDialog::onFieldChanged);
    connect(ui_->assetClassCombo,
            &QComboBox::currentIndexChanged,
            this,
            &InstrumentCodeDetailDialog::onFieldChanged);
    connect(ui_->oreTradeTypeEdit,
            &QLineEdit::textChanged,
            this,
            &InstrumentCodeDetailDialog::onFieldChanged);
}

void InstrumentCodeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateAssetClassCombo();
    setup_badge_combo(this, ui_->assetClassCombo, badgeCache(), "asset_class");
}

void InstrumentCodeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void InstrumentCodeDetailDialog::setCode(const refdata::domain::instrument_code& code_) {
    code__ = code_;
    updateUiFromCode();
}

void InstrumentCodeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void InstrumentCodeDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void InstrumentCodeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->assetClassCombo->setEnabled(!readOnly);
    ui_->oreTradeTypeEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void InstrumentCodeDetailDialog::populateAssetClassCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating asset_class combo";
    populateDynamicCombo<refdata::domain::asset_class_code>(
        ui_->assetClassCombo,
        this,
        clientManager_,
        &fetch_asset_class_codes,
        "assetClassWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(code__.asset_class); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load asset class codes: %1").arg(error));
        },
        [this]() { setup_badge_combo(this, ui_->assetClassCombo, badgeCache(), "asset_class"); },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void InstrumentCodeDetailDialog::updateUiFromCode() {
    ui_->codeEdit->setText(QString::fromStdString(code__.code));
    ui_->nameEdit->setText(QString::fromStdString(code__.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(code__.description));
    {
        const auto val = QString::fromStdString(code__.asset_class);
        const int idx = ui_->assetClassCombo->findData(val);
        if (idx >= 0)
            ui_->assetClassCombo->setCurrentIndex(idx);
    }
    ui_->oreTradeTypeEdit->setText(
        code__.ore_trade_type ? QString::fromStdString(*code__.ore_trade_type) : QString{});
    ui_->displayOrderEdit->setValue(code__.display_order);

    populateProvenance(code__.version,
                       code__.modified_by,
                       code__.performed_by,
                       code__.recorded_at,
                       code__.change_reason_code,
                       code__.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void InstrumentCodeDetailDialog::updateCodeFromUi() {
    if (createMode_) {
        code__.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    code__.name = ui_->nameEdit->text().trimmed().toStdString();
    code__.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    code__.asset_class = ui_->assetClassCombo->currentText().toStdString();
    {
        const auto ore_trade_type_str = ui_->oreTradeTypeEdit->text().trimmed().toStdString();
        code__.ore_trade_type = ore_trade_type_str.empty() ?
                                    std::nullopt :
                                    std::optional<std::string>(ore_trade_type_str);
    }
    code__.display_order = ui_->displayOrderEdit->value();
    code__.modified_by = username_;
}

void InstrumentCodeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void InstrumentCodeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void InstrumentCodeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool InstrumentCodeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();
    const bool asset_class_selected = ui_->assetClassCombo->currentIndex() >= 0;

    return true && !code_val.isEmpty() && !name_val.isEmpty() && asset_class_selected;
}

void InstrumentCodeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save instrument code while disconnected from server.");
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
    code__.change_reason_code = crSel->reason_code;
    code__.change_commentary = crSel->commentary;

    updateCodeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving instrument code: " << code__.code;

    QPointer<InstrumentCodeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, code_ = code__]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_instrument_code_request request;
        request.data = code_;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Instrument Code saved successfully";
            QString code = QString::fromStdString(self->code__.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->code_Saved(code);
            self->notifySaveSuccess(tr("Instrument Code '%1' saved").arg(code));
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

void InstrumentCodeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete instrument code while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(code__.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Instrument Code",
        QString("Are you sure you want to delete instrument code '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting instrument code: " << code__.code;

    QPointer<InstrumentCodeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = code__.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_instrument_code_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Instrument Code deleted successfully";
            emit self->statusMessage(QString("Instrument Code '%1' deleted").arg(code));
            emit self->code_Deleted(code);
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
