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
#include "ores.qt/SyntheticBindingDialog.hpp"
#include "ores.marketdata.api/domain/feed_binding.hpp"
#include "ores.marketdata.api/messaging/feed_binding_protocol.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include <QDialogButtonBox>
#include <QFutureWatcher>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <algorithm>

namespace ores::qt {

using namespace ores::logging;

namespace {

// Mirrors ir_curve_feed.cpp's own strip_currency_prefix() (also duplicated client-side in
// IrCurveEditor.cpp for the same reason: service-layer code isn't linkable from Qt). index_name
// is the full floating_index_type code (e.g. "USD-LIBOR-3M") -- strip the redundant
// "<CCY>-" prefix back off to build the same market_series qualifier ir_curve_feed uses.
std::string strip_currency_prefix(const std::string& currency_code, const std::string& index_name) {
    const auto prefix = currency_code + "-";
    if (index_name.starts_with(prefix))
        return index_name.substr(prefix.size());
    return index_name;
}

}

SyntheticBindingDialog::SyntheticBindingDialog(ClientManager* clientManager,
                                               const std::string& username,
                                               const std::vector<std::string>& existingSourceNames,
                                               QWidget* parent)
    : QDialog(parent)
    , clientManager_(clientManager)
    , username_(username)
    , existingSourceNames_(existingSourceNames)
    , table_(new QTableWidget(this))
    , createButton_(new QPushButton(tr("Create bindings"), this)) {

    setWindowTitle(tr("Create bindings from synthetic feeds"));
    setMinimumSize(700, 400);

    auto* layout = new QVBoxLayout(this);

    layout->addWidget(
        new QLabel(tr("Select synthetic FX/IR curve feeds to create feed bindings for. "
                      "Rows already bound are pre-checked and will be skipped."),
                  this));

    table_->setColumnCount(4);
    table_->setHorizontalHeaderLabels({tr(""), tr("Type"), tr("ORE Key"), tr("Source name")});
    table_->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    table_->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    table_->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Stretch);
    table_->setSelectionBehavior(QAbstractItemView::SelectRows);
    table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    table_->verticalHeader()->setVisible(false);
    layout->addWidget(table_);

    auto* btnRow = new QHBoxLayout;
    auto* selectAll = new QPushButton(tr("Select all"), this);
    auto* selectNone = new QPushButton(tr("Select none"), this);
    btnRow->addWidget(selectAll);
    btnRow->addWidget(selectNone);
    btnRow->addStretch();
    layout->addLayout(btnRow);

    auto* box = new QDialogButtonBox(this);
    createButton_->setDefault(true);
    box->addButton(createButton_, QDialogButtonBox::AcceptRole);
    box->addButton(QDialogButtonBox::Cancel);
    layout->addWidget(box);

    connect(selectAll, &QPushButton::clicked, this, &SyntheticBindingDialog::onSelectAllClicked);
    connect(selectNone, &QPushButton::clicked, this, &SyntheticBindingDialog::onSelectNoneClicked);
    connect(createButton_, &QPushButton::clicked, this, &SyntheticBindingDialog::onCreateClicked);
    connect(box, &QDialogButtonBox::rejected, this, &QDialog::reject);

    loadConfigs();
}

void SyntheticBindingDialog::loadConfigs() {
    table_->setEnabled(false);
    createButton_->setEnabled(false);

    auto* cm = clientManager_;
    QPointer<SyntheticBindingDialog> self = this;

    using Result = std::pair<bool, std::vector<BindableConfig>>;
    auto task = [cm]() -> Result {
        std::vector<BindableConfig> configs;

        synthetic::messaging::get_fx_spot_generation_configs_request fx_req;
        auto fx_resp = cm->process_authenticated_request(fx_req);
        if (!fx_resp)
            return {false, {}};
        for (const auto& cfg : fx_resp->fx_spot_generation_configs)
            configs.push_back({"FX", cfg.ore_key, cfg.source_name});

        synthetic::messaging::get_ir_curve_generation_configs_request ir_req;
        auto ir_resp = cm->process_authenticated_request(ir_req);
        if (!ir_resp)
            return {false, {}};
        for (const auto& cfg : ir_resp->ir_curve_generation_configs) {
            const auto ore_key =
                cfg.currency_code + "/" + strip_currency_prefix(cfg.currency_code, cfg.index_name);
            configs.push_back({"IR", ore_key, cfg.source_name});
        }

        return {true, std::move(configs)};
    };

    auto* watcher = new QFutureWatcher<Result>(this);
    connect(watcher, &QFutureWatcher<Result>::finished, this, [self, watcher]() {
        auto [ok, configs] = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;
        if (!ok) {
            MessageBoxHelper::critical(self,
                                       self->tr("Load failed"),
                                       self->tr("Could not load synthetic FX/IR curve configs."));
            self->reject();
            return;
        }
        self->configs_ = std::move(configs);
        self->populateTable(self->configs_);
        self->table_->setEnabled(true);
        self->createButton_->setEnabled(true);
    });
    watcher->setFuture(QtConcurrent::run(task));
}

void SyntheticBindingDialog::populateTable(const std::vector<BindableConfig>& configs) {
    table_->setRowCount(static_cast<int>(configs.size()));

    for (int row = 0; row < static_cast<int>(configs.size()); ++row) {
        const auto& cfg = configs[row];

        auto* chk = new QTableWidgetItem;
        chk->setFlags(Qt::ItemIsUserCheckable | Qt::ItemIsEnabled);

        const bool alreadyBound =
            std::ranges::find(existingSourceNames_, cfg.source_name) != existingSourceNames_.end();
        chk->setCheckState(alreadyBound ? Qt::Checked : Qt::Unchecked);
        if (alreadyBound)
            chk->setToolTip(tr("Already bound — will be skipped"));

        table_->setItem(row, 0, chk);
        table_->setItem(row, 1, new QTableWidgetItem(QString::fromStdString(cfg.kind)));
        table_->setItem(row, 2, new QTableWidgetItem(QString::fromStdString(cfg.ore_key)));
        table_->setItem(row, 3, new QTableWidgetItem(QString::fromStdString(cfg.source_name)));
    }
}

void SyntheticBindingDialog::onSelectAllClicked() {
    for (int r = 0; r < table_->rowCount(); ++r)
        if (auto* chk = table_->item(r, 0))
            chk->setCheckState(Qt::Checked);
}

void SyntheticBindingDialog::onSelectNoneClicked() {
    for (int r = 0; r < table_->rowCount(); ++r)
        if (auto* chk = table_->item(r, 0))
            chk->setCheckState(Qt::Unchecked);
}

void SyntheticBindingDialog::onCreateClicked() {
    std::vector<BindableConfig> selected;
    for (int r = 0; r < table_->rowCount(); ++r) {
        auto* chk = table_->item(r, 0);
        if (!chk || chk->checkState() != Qt::Checked)
            continue;
        if (r >= static_cast<int>(configs_.size()))
            continue;
        const auto& cfg = configs_[r];
        // Skip rows that already have a binding.
        if (std::ranges::find(existingSourceNames_, cfg.source_name) != existingSourceNames_.end())
            continue;
        selected.push_back(cfg);
    }

    if (selected.empty()) {
        MessageBoxHelper::information(
            this, tr("Nothing to create"), tr("No new synthetic feeds selected."));
        return;
    }

    createBindings(selected);
}

void SyntheticBindingDialog::createBindings(const std::vector<BindableConfig>& selected) {
    createButton_->setEnabled(false);
    table_->setEnabled(false);

    auto* cm = clientManager_;
    const std::string username = username_;
    QPointer<SyntheticBindingDialog> self = this;

    using SaveResult = std::vector<std::pair<std::string, bool>>;
    auto task = [cm, username, selected]() -> SaveResult {
        SaveResult results;
        for (const auto& cfg : selected) {
            marketdata::domain::feed_binding b;
            b.id = boost::uuids::random_generator()();
            b.ore_key = cfg.ore_key;
            b.source_name = cfg.source_name;
            b.enabled = true;
            b.performed_by = username;
            b.change_reason_code = "system.new_record";
            b.change_commentary = "Created from synthetic feed binding dialog";

            auto req = marketdata::messaging::save_feed_binding_request::from(std::move(b));

            auto resp = cm->process_authenticated_request(req);
            const bool ok = resp && resp->success;
            results.push_back({cfg.ore_key, ok});
        }
        return results;
    };

    auto* watcher = new QFutureWatcher<SaveResult>(this);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, this, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        int ok = 0, failed = 0;
        for (const auto& [key, success] : results) {
            if (success)
                ++ok;
            else
                ++failed;
        }

        self->bindingsCreated_ = ok;

        if (failed == 0) {
            self->accept();
        } else {
            MessageBoxHelper::warning(
                self,
                self->tr("Partial success"),
                self->tr("Created %1 binding(s); %2 failed. Check logs for details.")
                    .arg(ok)
                    .arg(failed));
            if (ok > 0)
                self->accept();
            else
                self->reject();
        }
    });
    watcher->setFuture(QtConcurrent::run(task));
}

}
