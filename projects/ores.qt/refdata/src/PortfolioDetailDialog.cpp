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
#include "ores.qt/PortfolioDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ui_PortfolioDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

PortfolioDetailDialog::PortfolioDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::PortfolioDetailDialog)
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

PortfolioDetailDialog::~PortfolioDetailDialog() {
    delete ui_;
}

QTabWidget* PortfolioDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* PortfolioDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* PortfolioDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString PortfolioDetailDialog::code() const {
    return QString::fromStdString(portfolio_.name);
}

void PortfolioDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PortfolioDetailDialog::setupCombos() {
    ui_->statusCombo->clear();
    ui_->statusCombo->addItem(tr("Active"), QString("Active"));
    ui_->statusCombo->addItem(tr("Inactive"), QString("Inactive"));
    ui_->statusCombo->addItem(tr("Closed"), QString("Closed"));
    ui_->statusCombo->addItem(tr("Frozen"), QString("Frozen"));
    ui_->statusCombo->addItem(tr("Pending"), QString("Pending"));
}

void PortfolioDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &PortfolioDetailDialog::onSaveClicked);
    connect(
        ui_->deleteButton, &QPushButton::clicked, this, &PortfolioDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &PortfolioDetailDialog::onCloseClicked);

    connect(ui_->idEdit, &QLineEdit::textChanged, this, &PortfolioDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->purposeTypeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->statusCombo,
            &QComboBox::currentIndexChanged,
            this,
            &PortfolioDetailDialog::onFieldChanged);
    connect(
        ui_->isVirtualCheckBox, &QCheckBox::toggled, this, &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->aggregationCcyEdit,
            &QComboBox::currentIndexChanged,
            this,
            &PortfolioDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &PortfolioDetailDialog::onFieldChanged);
}

void PortfolioDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populatePurposeTypeCombo();
    populateAggregationCcyCombo();
}

void PortfolioDetailDialog::populateAggregationCcyCombo() {
    setup_currency_combo(ui_->aggregationCcyEdit, this, clientManager_, imageCache(), [this]() {
        return QString::fromStdString(portfolio_.aggregation_ccy);
    });
}

void PortfolioDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PortfolioDetailDialog::setPortfolio(const refdata::domain::portfolio& portfolio) {
    portfolio_ = portfolio;
    updateUiFromPortfolio();
}

void PortfolioDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(true);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        portfolio_.id = boost::uuids::random_generator()();
        if (clientManager_)
            portfolio_.party_id = clientManager_->currentPartyId();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void PortfolioDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PortfolioDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->purposeTypeCombo->setEnabled(!readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
    ui_->aggregationCcyEdit->setEnabled(!readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PortfolioDetailDialog::populatePurposeTypeCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating purpose_type combo";
    populateDynamicCombo<refdata::domain::purpose_type>(
        ui_->purposeTypeCombo,
        this,
        clientManager_,
        &fetch_purpose_types,
        "purposeTypeWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(portfolio_.purpose_type); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load purpose types: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void PortfolioDetailDialog::updateUiFromPortfolio() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(portfolio_.id)));
    ui_->nameEdit->setText(QString::fromStdString(portfolio_.name));
    {
        const auto val = QString::fromStdString(portfolio_.purpose_type);
        const int idx = ui_->purposeTypeCombo->findData(val);
        if (idx >= 0)
            ui_->purposeTypeCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(portfolio_.status);
        const int idx = ui_->statusCombo->findData(val);
        if (idx >= 0)
            ui_->statusCombo->setCurrentIndex(idx);
    }
    ui_->isVirtualCheckBox->setChecked(portfolio_.is_virtual);
    {
        const auto val = QString::fromStdString(portfolio_.aggregation_ccy);
        const int idx = ui_->aggregationCcyEdit->findText(val);
        ui_->aggregationCcyEdit->setCurrentIndex(idx);
    }
    ui_->descriptionEdit->setPlainText(QString::fromStdString(portfolio_.description));

    populateProvenance(portfolio_.version,
                       portfolio_.modified_by,
                       portfolio_.performed_by,
                       portfolio_.recorded_at,
                       portfolio_.change_reason_code,
                       portfolio_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PortfolioDetailDialog::updatePortfolioFromUi() {
    portfolio_.name = ui_->nameEdit->text().trimmed().toStdString();
    portfolio_.purpose_type = ui_->purposeTypeCombo->currentText().toStdString();
    portfolio_.status = ui_->statusCombo->currentData().toString().toStdString();
    portfolio_.is_virtual = ui_->isVirtualCheckBox->isChecked();
    portfolio_.aggregation_ccy = ui_->aggregationCcyEdit->currentText().toStdString();
    portfolio_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    portfolio_.modified_by = username_;
}

void PortfolioDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PortfolioDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PortfolioDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PortfolioDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !id_val.isEmpty() && !name_val.isEmpty();
}

void PortfolioDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save portfolio while disconnected from server.");
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
    portfolio_.change_reason_code = crSel->reason_code;
    portfolio_.change_commentary = crSel->commentary;

    updatePortfolioFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving portfolio: " << portfolio_.name;

    QPointer<PortfolioDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, portfolio = portfolio_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_portfolio_request request;
        request.data = portfolio;
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
            BOOST_LOG_SEV(lg(), info) << "Portfolio saved successfully";
            QString code = QString::fromStdString(self->portfolio_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->portfolioSaved(code);
            self->notifySaveSuccess(tr("Portfolio '%1' saved").arg(code));
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

void PortfolioDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete portfolio while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(portfolio_.name);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Portfolio",
        QString("Are you sure you want to delete portfolio '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting portfolio: " << portfolio_.name;

    QPointer<PortfolioDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(portfolio_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_portfolio_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Portfolio deleted successfully";
            emit self->statusMessage(QString("Portfolio '%1' deleted").arg(code));
            emit self->portfolioDeleted(code);
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
