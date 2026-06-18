// Authors: RB, JP

data {
    int<lower=1> N_games;
    int<lower=2> N_teams;
    vector[N_games] y;
    array[N_games] int<lower=1, upper=N_teams> H;
    array[N_games] int<lower=1, upper=N_teams> A;
}

parameters {
    real home_field;
    vector[N_teams] strength_raw;
    real<lower=0> sigma_team;
    real<lower=0> sigma_game;
}

transformed parameters {
    vector[N_teams] strength;
    strength = sigma_team * (strength_raw - mean(strength_raw));
}

model {
    home_field ~ normal(0, 5);
    strength_raw ~ std_normal();
    sigma_team ~ normal(0, 7);
    sigma_game ~ normal(0, 20);

    y ~ normal(home_field + strength[H] - strength[A], sigma_game);
}

generated quantities {
    vector[N_games] y_rep;

    for (i in 1:N_games) {
        y_rep[i] = normal_rng(
            home_field + strength[H[i]] - strength[A[i]],
            sigma_game
        );
    }
}
