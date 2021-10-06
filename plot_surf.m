clear;

syms x_0 x_1;

f = sqrt(x_0 ^ 2 + x_1 ^ 2) - 2;
objective = f^2;

h = matlabFunction(objective);
dx_0 = matlabFunction(diff(f, x_0));
dx_1 = matlabFunction(diff(f, x_1));

spacing = 0.2; 
[X, Y] = meshgrid(-4:spacing:4);
Z = h(X, Y);
[DX, DY] = gradient(Z, spacing);

%% Draw

contourf(X, Y, Z);
hold on;
quiver(X, Y, DX, DY, 'k', 'LineWidth', 2);
